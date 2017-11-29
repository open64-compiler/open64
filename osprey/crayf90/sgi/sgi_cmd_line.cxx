/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



static const char *source_file = __FILE__;
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/crayf90/sgi/sgi_cmd_line.cxx,v $ $Revision: 1.1.1.1 $";

#include <stdint.h>
/* SGI includes */
#include "stamp.h"
#include "defs.h"
#include "config.h"  
#include "config_debug.h"  
#include "file_util.h"
#include "flags.h"
#include "glob.h"
#include "tracing.h"
#include "util.h" 
#include "wn.h"

#include "erglob.h"
#include "erlib.h"
#include "tracing.h"
#include "err_host.tab"
#ifdef KEY /* Bug 4607 */
#  include "path_intrinsic_list.h"
#endif /* KEY Bug 4607 */
#ifdef KEY /* Bug 4260 */
#include "../../../clibinc/cray/io_byteswap.h"
#endif /* KEY Bug 4260 */
#ifdef KEY /* Bug 6204 */
#include "decorate_utils.h"
#endif /* KEY Bug 6204 */

/* conversion includes */
#include "sgi_cmd_line.h"
#include "cwh_mkdepend.h"

/* Get the compiler build data */
#include "version.h"

#include <cmplrs/make_depend.h> 
#include <stdarg.h>
#include <signal.h>

BOOL IO_Comments = TRUE;  /* Emit I/O comments from lowerer */
BOOL Use_Three_Call= FALSE;

BOOL FE_Full_Split_Set = FALSE;
BOOL FE_Full_Split     = FALSE;
BOOL FE_Endloop_Marker = FALSE;

static BOOL Matmul_Inline = FALSE;
static BOOL Mv_Matmul_Inline = FALSE;
static BOOL Matmul_Inline_Set = FALSE;
static BOOL Mv_Matmul_Inline_Set = FALSE;

char *rii_file_name=NULL;
BOOL enable_dsm_recompile = FALSE;
BOOL enable_dsm_processing = FALSE;
static BOOL enable_mp_processing = FALSE;

BOOL Full_arrayexp = TRUE;
BOOL Full_arrayexp_set = FALSE;
mUINT16  FE_align=8;

char *FE_gdar_filename = NULL;
#ifdef KEY
char *F2C_ABI_filename = NULL;
#endif
#ifdef KEY /* Bug 3507 */
BOOL option_underscoring = TRUE;
BOOL option_second_underscore = TRUE;
#endif /* KEY Bug 3507 */

INT32 global_chunk_pragma_value;
BOOL  global_chunk_pragma_set = FALSE;

INT32 global_schedtype_pragma_val;
BOOL global_schedtype_pragma_set = FALSE;

BOOL process_cri_mp_pragmas=FALSE;

#ifdef KEY
BOOL disable_old_mp = TRUE;		// bug 4406
#else
BOOL disable_old_mp = FALSE;
#endif
BOOL disable_open_mp = FALSE;

BOOL FE_Call_Never_Return = TRUE;


/*
 *
 * Mempools
 *
 */
static MEM_POOL FE_Mempool_s;
MEM_POOL *FE_Mempool=&FE_Mempool_s;

#ifdef KEY /* Bug 4719 */
char *preprocessor_output_file;
#endif /* KEY Bug 4719 */
#ifdef KEY /* Bug 4260 */
int io_byteswap = IO_DEFAULT;
#endif /* KEY Bug 4260 */

/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

/*       MAX_DEBUG_LEVEL	2  :: Defined in flags.h */
# define DEF_DEBUG_LEVEL	0
INT8 Debug_Level = DEF_DEBUG_LEVEL;	/* -gn:	debug level */
# define MAX_PROFILE_LEVEL 0
# define DEF_PROFILE_LEVEL 0
INT8 Profile_Level = DEF_PROFILE_LEVEL;	/* -pn:	profiling level	*/
# define MAX_MSG_LEVEL 2
# define DEF_MSG_LEVEL 2



static OPTION_DESC Options_FE[] = {
  { OVK_NAME,	OV_INTERNAL,  	FALSE, "cmdline",		"cmdline",
    0, 0, 0, &FE_command_line, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, 	"iocomment",		"ioc",
    0, 0, 0, &IO_Comments  , NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "three_call",		"three_call",
    0, 0, 0,    &Use_Three_Call, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "full_split",		"full",
    0, 0, 0,	&FE_Full_Split,		&FE_Full_Split_Set },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "arrayexp",		"arrayexp",
    0, 0, 0,	&Full_arrayexp,		&Full_arrayexp_set },
  { OVK_NAME,   OV_INTERNAL,    FALSE, "gdar",                   "gdar",
    0, 0, 0, &FE_gdar_filename,        NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "endloop_markers",	"endloop_marker",
    0, 0, 0,	&FE_Endloop_Marker,	NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "matmul_inline",	        "matmul_inline",
    0, 0, 0,	&Matmul_Inline,	&Matmul_Inline_Set },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "mv_matmul_inline",	"mv_matmul_inline",
    0, 0, 0,	&Mv_Matmul_Inline,	&Mv_Matmul_Inline_Set },
  { OVK_BOOL,   OV_INTERNAL,    FALSE, "call_never_return", "call_never_return",
    0, 0, 0,    &FE_Call_Never_Return,  NULL },
  { OVK_COUNT }         /* List terminator -- must be last */
};

OPTION_GROUP FE_Option_Groups [] = {
  { "FE", ':', '=', Options_FE },
  { NULL }                         /* List terminator -- must be last */
};


INT argc_cray;
char **argv_cray;
INT size_cray_argv;
INT num_cray_args;

char **deferred_argv_cray;
INT size_deferred_cray_argv;
INT num_deferred_cray_args;

char **save_argv;
INT save_argc;

static char *object_file_name=NULL;

static WN_PRAGMA_SCHEDTYPE_KIND
identify_schedtype(char *text)
{
   if (strncasecmp(text, "simple",6)==0  ||
       strncasecmp(text, "static",6)==0) {
      return WN_PRAGMA_SCHEDTYPE_SIMPLE;
   } else if (strncasecmp(text, "dynamic",7)==0) {
      return WN_PRAGMA_SCHEDTYPE_DYNAMIC;
   } else if (strncasecmp(text, "interleaved",11)==0) {
      return WN_PRAGMA_SCHEDTYPE_INTERLEAVE;
   } else if ( strncasecmp(text, "interleave",10)==0) {
      return WN_PRAGMA_SCHEDTYPE_INTERLEAVE;
   } else if (strncasecmp(text, "runtime",7)==0) {
      return WN_PRAGMA_SCHEDTYPE_RUNTIME;
   } else if (strncasecmp(text, "gss",3)==0) {
      return WN_PRAGMA_SCHEDTYPE_GSS;
   } else if (strncasecmp(text, "guided",6)==0) {
      return WN_PRAGMA_SCHEDTYPE_GSS;
   } else if (strncasecmp(text, "pseudolowered",13)==0) {
      return WN_PRAGMA_SCHEDTYPE_PSEUDOLOWERED;
   } else {
      ErrMsg(EC_Unknown_Mpsched,text);
      return WN_PRAGMA_SCHEDTYPE_UNKNOWN;
   }
} 


void add_cray_args(const char *arg)
{
   /* Add a string to the Cray args */

   if (num_cray_args >= size_cray_argv) {
      /* Reallocate argv_cray */
      size_cray_argv += 16;
      argv_cray = (char **) realloc(argv_cray,size_cray_argv*sizeof(char *));
   }
   
   if (arg) {
      argv_cray[num_cray_args] = strdup(arg);
   } else {
      argv_cray[num_cray_args] = NULL;
   }
   ++num_cray_args;
}

void add_deferred_cray_args(char *arg)
{
   /* Add a string to the deferred Cray args */

   if (num_deferred_cray_args >= size_deferred_cray_argv) {
      /* Reallocate argv_cray */
      size_deferred_cray_argv += 16;
      deferred_argv_cray = (char **) realloc(deferred_argv_cray,
					     size_deferred_cray_argv*sizeof(char *));
   }
   
   if (arg) {
      deferred_argv_cray[num_deferred_cray_args] = strdup(arg);
   } else {
      deferred_argv_cray[num_deferred_cray_args] = NULL;
   }      
   ++num_deferred_cray_args;
}

/* ====================================================================
 *
 * Cray_Woff
 *
 * Process a set of warning numbers which are to be disabled.  They are
 * given in the form:
 *
 *	n1,n2,n3-n4,n5-n6,...
 *
 * where an individual number (e.g. n1 or n2) suppresses that numbered
 * warning, and a range (e.g. n3-n4 or n5-n6) disables all warnings
 * numbered in that range.
 *
 * This routine takes such a number list from -DEBUG:woff=... or from
 * -woff ..., converts it to the Cray format option, and passes it to
 * the Cray command line option handler.
 *
 * ====================================================================
 */

void
Cray_Woff ( char *list )
{
   char *temp = (char *) malloc ( strlen(list)+3 );
   
   strcpy ( temp, "-M" );
   strcat ( temp, list );
   add_cray_args ( temp );
   free ( temp );
}

/* ====================================================================
 *
 * Process_Command_Line
 *
 * Process the command line arguments.	Evaluate all flags except per-
 * source file control flags and set up	global options.
 *
 * ====================================================================
 */

void Process_Command_Line (INT argc, char ** argv)

{
   INT i;
   char *cp;
   char *temp;
   char ch,c;
   BOOL pass_option;
#ifdef KEY /* Bug 10177 */
   BOOL opt_set = FALSE;
#else /* KEY Bug 10177 */
   BOOL opt_set;
#endif /* KEY Bug 10177 */
   INT j;
   FILE *f;
   INT  command_line_length;

   argc_cray = argc;
   num_cray_args = 1;
   size_cray_argv = argc + 1;
   
   /* Get the space for (potential) copies of all the command-line options */
   argv_cray = (char **) malloc(size_cray_argv*sizeof(char *));
   argv_cray[0] = argv[0];

   /* Get the space for the deferred cray args */
   num_deferred_cray_args=0;
   size_deferred_cray_argv = 1; /* Pretty arbitrary */
   deferred_argv_cray = (char **)malloc(size_deferred_cray_argv*sizeof(char *));

   /* Process the command line */
   for (i=1; i<argc; i++ ) {
      if ( argv[i] != NULL && *(argv[i]) == '-' )	{
	 cp = argv[i]+1;	/* Pointer to next flag	character */
	 if (Process_Command_Line_Group(cp, Common_Option_Groups)) {
	    pass_option = FALSE;
#ifdef KEY /* Bug 3405 */
            if (LANG_IEEE_Minus_Zero_On) {
	      add_cray_args("-ez");
	    }
#else
            switch ( *cp++ ) {
              case 'L':
               if ((strncmp(cp, "ANG:IEEE_minus_zero", 19) == 0) &&
                   (strncmp(cp, "ANG:IEEE_minus_zero=off", 23) != 0) &&
                   (strncmp(cp, "ANG:IEEE_minus_zero=OFF", 23) != 0)) {
                  add_cray_args("-ez");
               }
            }
#endif /* KEY Bug 3405 */
         } else if (Process_Command_Line_Group(cp, FE_Option_Groups)) {
	    pass_option = FALSE;
	 } else {
	    pass_option = TRUE;
	    switch ( *cp++ ) {
	     case 'a':
	       if (strcmp(cp,"lign32")==0) {
		  pass_option = TRUE;
		  FE_align = 4;
	       } else if (strcmp(cp,"lign64")==0) {
		  pass_option = TRUE;
		  FE_align = 8;
	       } else if (strcmp(cp,"nsi")==0) {
		  pass_option = FALSE;
		  add_cray_args("-en");
	       } else if (strncmp(cp,"uto_use=",8)==0) {
		  cp += 8;
		  temp = (char *) malloc (strlen(cp) + 3);
		  strcpy(temp,"-A");
		  strcat(temp,cp);
		  add_cray_args(temp);
		  free(temp);
		  pass_option = FALSE;
	       }
	       break;

#ifdef KEY /* Bug 4260 */
	     case 'b':
	       if (strcmp(cp, "yteswapio")==0) {
		  io_byteswap = IO_SWAP;
	          pass_option = FALSE;
	       }
	       break;
#endif /* KEY Bug 4260 */

	     case 'c':
	       if (strcmp(cp,"ol72")==0) {
		  pass_option = FALSE;
		  add_cray_args("-N72");
	       } else if (strcmp(cp,"ol80")==0) {
		  pass_option = FALSE;
		  add_cray_args("-N80");
	       } else if (strcmp(cp,"ol120")==0) {
		  pass_option = FALSE;
		  add_cray_args("-N120");
	       } else if (strcmp(cp,"if")==0) {
		  pass_option = FALSE;
		  add_cray_args("-Ca");
	       } else if (strncmp(cp,"hunk=",5)==0) {
		  cp += 5;
		  global_chunk_pragma_set = TRUE;
		  global_chunk_pragma_value = Get_Numeric_Flag ( &cp, 0, INT32_MAX,0,argv[i] );
		  pass_option = FALSE;
	       } else if (strcmp(cp,"ray_mp")==0) {
		 pass_option = FALSE;
		 process_cri_mp_pragmas = TRUE;
		 add_cray_args("-Otask1");
               }
#ifdef KEY /* Bug 4260 */
	       else if (strcmp(cp,"onvert")==0) {
		 pass_option = FALSE;
		 cp = argv[++i]; /* Get next argument */
		 if (strcmp(cp, "native")==0) {
		   io_byteswap = IO_NATIVE;
		 } else if (strcmp(cp, "big_endian")==0 ||
		   strcmp(cp, "big-endian")==0) {
		   io_byteswap = IO_BIG_ENDIAN;
		 } else if (strcmp(cp, "little_endian")==0 ||
		   strcmp(cp, "little-endian")==0) {
		   io_byteswap = IO_LITTLE_ENDIAN;
		 } else {
		   /* No way to report error here, so back up and pass -convert
		    * to the Cray front end which will complain about that. Not
		    * perfect, but the driver should intercept errors before we
		    * reach this spot anyway.
		    */
		   cp = argv[--i] + 1;
		   pass_option = TRUE;
		 }
               }
#endif /* KEY Bug 4260 */
	       break;

	     case 'd':
	       if (strcmp(cp,"16")==0) {
		  pass_option = FALSE;
		  add_cray_args("-sdoubleprecision16");
		  add_cray_args("-sdoublecomplex16");
	       } else if (strcmp(cp,"8")==0) {
		  pass_option = FALSE;
	       } else if (strcmp(cp,"sm")==0) {
		  /* -dsm: enable the DSM directives */
		  enable_dsm_processing = TRUE;
		  add_cray_args("-udsm");
		  pass_option = FALSE;
	       } else if (strcmp(cp,"sm_recompile")==0) {
			/* -dsm_recompile: do not update rii file */
		  enable_dsm_recompile = TRUE;
		  pass_option = FALSE;
	       } else if (strcmp(cp,"efault64")==0) {
		  pass_option = FALSE;
		  add_cray_args("-ucray_compatible");
               } else if (strcmp(cp,"isable_old_mp")==0) {
                  pass_option = FALSE;
                  disable_old_mp = TRUE;
               } else if (strcmp(cp,"isable_open_mp")==0) {
                  pass_option = FALSE;
                  disable_open_mp = TRUE;
               }
               else if (strcmp(cp,"_lines")==0) {
                  pass_option = FALSE;
                  add_cray_args("-ed");
               }
	       break;

	     case 'e':
	       if (strcmp(cp,"xtend_source")==0) {
		  pass_option = FALSE;
		  add_cray_args("-N132");
	       }
	       break;

             case 'E':
                pass_option = FALSE;
                add_cray_args("-eZ");
                break;
	       
	     case 'f':
	       if ( strcmp(cp,"ullwarn") == 0 ) {
		  /* -fullwarn -- similar to -m2: */
		  Min_Error_Severity = ES_ADVISORY; 
		  add_cray_args("-m2");
		  pass_option = FALSE;
               } else if ( strcmp(cp,"tpp") == 0 ) {
                  pass_option = FALSE;
                  add_cray_args("-eT");
               } else if ( strcmp(cp,"no-second-underscore") == 0 ) {
                  add_cray_args("-dN");
                  pass_option = FALSE;
#ifdef KEY /* Bug 3507 */
		  option_second_underscore = FALSE;
#endif /* KEY Bug 3507 */
               } else if ( strcmp(cp,"second-underscore") == 0 ) {
                  add_cray_args("-eN");
                  pass_option = FALSE;
               } else if ( strcmp(cp,"no-underscoring") == 0 ) {
                  add_cray_args("-dO");
                  pass_option = FALSE;
#ifdef KEY /* Bug 3507 */
		  option_underscoring = FALSE;
#endif /* KEY Bug 3507 */
               } else if ( strcmp(cp,"underscoring") == 0 ) {
                  add_cray_args("-eO");
                  pass_option = FALSE;
#ifdef KEY
	       } else if ( strcmp(cp,"f2c-abi") == 0 ) {
		  F2C_ABI_filename = strdup ( argv[i+1] );
		  i++;
	          pass_option = FALSE;
#endif
#ifdef KEY /* Bug 6204 */
	       } else if ( strcmp(cp,"decorate") == 0 ) {
		  parse_decorate_script(argv[i+1]);
		  i++;
	          pass_option = FALSE;
#endif /* KEY Bug 6204 */
	       } else { /* Filename options -- ignore these except -fb: */
		  c = *cp++;
		  if ( (ch=*cp++) != ',' && ch != ':' ) {
		     if ( c == 0 ) c = '?';
		     pass_option = TRUE;
		     break;
		  }
		  /* Process the various options: */
		  pass_option = FALSE;
		  switch ( c ) {
		     
		   case 'b': /* Library file already processed -- ignore: */
		     Lib_File_Name = strdup(cp);
		     break;
		     
		   case 'B': /* WHIRL file: */
		     Irb_File_Name = strdup(cp);
		     break;

		   case 'C': /* CIF file */
		      temp = (char *) malloc (strlen(cp) + 4);
		      strcpy(temp,"-CZ");
		      strcat(temp,cp);
		      add_cray_args(temp);
		      free(temp);
		      pass_option = FALSE;
		      break;
		     
		   case 'D': /* Cray debug file */
#ifdef KEY	      // Bug 933.
		      // Handle spaces in file names.
		      {
			char *p, *t;
			temp = (char *) malloc (strlen(cp) * 2 + 8);
			strcpy(temp,"-ufile=");
			t = temp + 7;
			// Replace ' ' with '\ '.  Replace '\' with '\\'.
			for (p = cp; *p; p++) {
			  if (*p == ' ' || *p == '\\')
			    *t++ = '\\';
			  *t++ = *p;
			}
			*t = '\0';
		      }
#else
		      temp = (char *) malloc (strlen(cp) + 8);
		      strcpy(temp,"-ufile=");
		      strcat(temp,cp);
#endif
		      add_cray_args(temp);
		      free(temp);
		      pass_option = FALSE;
		      break;
		     
		   case 'S': /* Original source file name (used for -mp option) */
		     Orig_Src_File_Name = strdup(cp);
		     break;
		     
		   case 'e': /* Error file: */
		     Err_File_Name = strdup(cp);
		     break;
		     
		   case 'I': /* Ipa_File_Name: ignore it */
		     break;
		     
		   case 'l': /* Listing file: */
		     Lst_File_Name = strdup(cp);
		     break;

		   case 'o': /* Object file: */
		     object_file_name = strdup(cp);
		     break;

		   case 't': /* Error file: */
		     Tracing_Enabled = TRUE;
		     Trc_File_Name = strdup(cp);
		     break;

		   default:
		     pass_option = TRUE;
		  }
	       }
	       break;
	       
	     case 'g':		/* Debug level: */
	       Debug_Level = Get_Numeric_Flag
		 ( &cp, 0, MAX_DEBUG_LEVEL,
		  2, argv[i] );
	       if (!opt_set)
		 Opt_Level = 0;
               pass_option = FALSE;
               add_cray_args("-Gd");
	       break;

	     case 'G':		/* Sdata elt size */
	       Max_Sdata_Elt_Size = Get_Numeric_Flag
		 ( &cp, 0, MAX_SDATA_ELT_SIZE,
		  DEF_SDATA_ELT_SIZE, argv[i] );
	       pass_option = FALSE;
	       break;
	     
	     case 'i':
	       if (strcmp(cp, "eee_minus_zero") == 0) {
		  add_cray_args("-ez");
                  pass_option = FALSE;
               }
	       else if ( strncmp(cp, "nclude=", 7) == 0) {
		  /* -include= */
		  cp += 7;
		  temp = (char *) malloc (strlen(cp) + 3);
		  strcpy(temp+2,cp);
		  temp[0] = '-'; temp[1] = 'I';
		  add_cray_args(temp);
		  /* Also add -p<path> to pick up the right module information */
		  temp[1] = 'p';
		  add_cray_args(temp);
		  free(temp);
		  pass_option = FALSE;
	       } else if (strcmp(cp,"4") == 0) {
		  pass_option = FALSE;
	       } else if (strcmp(cp,"8") == 0) {
		  add_cray_args("-sinteger8");
		  add_cray_args("-slogical8");
		  pass_option = FALSE;
	       }
#ifdef KEY /* Bug 4607 */
#  define NTRINSIC "ntrinsic="
#  define NTRINSIC_LEN ((sizeof NTRINSIC) - 1)
               else if (0 == strncmp(cp, NTRINSIC, NTRINSIC_LEN)) {
	          path_intrinsic_list.add(cp + NTRINSIC_LEN, 1);
		  pass_option = FALSE;
	       }
#endif /* KEY Bug 4607 */
		  
	       break;

	     case 'k':
	       if ( strcmp(cp,"eep") == 0 ) {
		  add_cray_args("-ek"); /* set save_dot_i = TRUE */
		  pass_option = FALSE;
               }

	       break;

	     case 'm': 
	       {
		  if (strcmp(cp,"p") == 0) {
                    pass_option = FALSE;
		    enable_mp_processing = TRUE;
                  } else if (strncmp(cp,"p_chunk=",8) == 0) {
		     cp += 8;
		     global_chunk_pragma_set = TRUE;
		     global_chunk_pragma_value = Get_Numeric_Flag ( &cp, 0, INT32_MAX,0,argv[i] );
		     pass_option = FALSE;
                  } else if (strncmp(cp,"p_schedtype=",12) == 0) {
		     cp += 12;
		     global_schedtype_pragma_val = identify_schedtype(cp);
		     if (global_schedtype_pragma_val !=  WN_PRAGMA_SCHEDTYPE_UNKNOWN) {
			global_schedtype_pragma_set = TRUE;
		     }
		     pass_option = FALSE;
		  } else {
		     int cray_num;
		     char temp[4];
		     /* Message reporting: */
		     cray_num = 3;
		     j = Get_Numeric_Flag ( &cp, 0, MAX_MSG_LEVEL,
					    MAX_MSG_LEVEL, argv[i] );
		     switch (j) {
		      case 0: 
			 Min_Error_Severity = ES_ERROR;
			 cray_num=4;
			 break;
		      case 1: 
			 Min_Error_Severity = ES_WARNING;  
			 cray_num = 3;
			 break;
		      case 2: 
			 cray_num=0;
			 Min_Error_Severity = ES_ADVISORY; 
			 break;
		     }
		     sprintf(temp,"-m%1d",cray_num);
		     add_cray_args(temp);
		     pass_option = FALSE;
		  }
	       }
	       break;
	     case 'M':
	       if (strcmp(cp, "Dtarget") == 0) {
		  mdtarget_file = strdup(argv[i+1]);
		  ++i;
		  pass_option = FALSE;
	       } else if (strcmp(cp, "Dupdate") == 0) {
		  mdupdate_file = strdup(argv[i+1]);
		  ++i;
		  pass_option = FALSE;
	       }
	       break;

	     case 'n':
	       if ( strcmp(cp, "owarn") == 0) {
		  /* -nowarn, edg's -w */
		  /* Suppress warnings */
		  Min_Error_Severity = ES_ERROR;
		  add_cray_args("-m4");
		  pass_option = FALSE;
	       } else if (strcmp(cp,"oextend_source")==0) {
		  add_cray_args("-N72");
		  pass_option = FALSE;
	       } else if (strcmp(cp,"ocpp")==0) {
                  add_cray_args("-dT");
		  pass_option = FALSE;
               } else if (strcmp(cp,"og77mangle")==0) {
                  add_cray_args("-dN");
                  pass_option = FALSE;
               }
#ifdef KEY /* Bug 4607 */
#  define OINTRINSIC "o-intrinsic="
#  define OINTRINSIC_LEN ((sizeof OINTRINSIC) - 1)
               else if (0 == strncmp(cp, OINTRINSIC, OINTRINSIC_LEN)) {
	          path_intrinsic_list.add(cp + OINTRINSIC_LEN, 0);
		  pass_option = FALSE;
	       }
#endif /* KEY Bug 4607 */
	       break;

#ifdef KEY /* Bug 4719 */
	     case 'o':
	       // We don't have access to the error machinery in the fe90
	       // directory, so reporting trouble is hard; hopefully the
	       // compiler driver diagnoses errors for us.
	       if ((i + 1) < argc) {
		 preprocessor_output_file = strdup(argv[++i]);
		 }
	       pass_option = FALSE;
	       break;
#endif /* KEY Bug 4719 */

	     case 'O':		/* Optimization level: */
	       Opt_Level = Get_Numeric_Flag
		 ( &cp, 0, MAX_OPT_LEVEL, DEF_O_LEVEL, argv[i] );
	       opt_set = TRUE;
	       pass_option = FALSE;
	       break;

             case 'p':
               if (strcmp(cp,"ad_char_literals")==0) {
                  pass_option = FALSE;
                  add_cray_args("-ec");
               }
               break;

             case 'P':
               pass_option = FALSE;
               add_cray_args("-dk"); /* set output_pound_lines = FALSE */ 
               break;

	     case 'r':
	       if (strcmp(cp,"4") == 0) {
		  pass_option = FALSE;
	       } else if (strcmp(cp,"8") == 0) {
		  add_cray_args("-sreal8");
		  add_cray_args("-scomplex8");
		  pass_option = FALSE;
	       } else if (strncmp(cp,"ii",2) == 0)
	       {
		  /* -rii : set rii_file_name */
		  cp += 2;
		  rii_file_name = strdup(cp);
		  pass_option = FALSE;
	       }
	       break;
	       
	     case 's': 
	       if ( strcmp(cp,"how") == 0) {
		  /* -show: Emit progress reports: */
		  Show_Progress = TRUE;
		  pass_option = FALSE;
	       } else if ( strcmp(cp,"tatic_threadprivate") == 0) {
		  /* -static_threadprivate */
		  add_cray_args("-astatic_threadprivate");
		  pass_option = FALSE;
	       } else if ( strcmp(cp,"tatic-data") == 0) {
		  /* -static-data, same as -static */
		  add_cray_args("-ev");
		  pass_option = FALSE;
	       } else if ( strcmp(cp,"tatic") == 0) {
		  /* -static */
		  add_cray_args("-ev");
		  pass_option = FALSE;
               }
	       break;

	     case 't':
	       /* Trace specification:
		* The config.c routine expects to get a pointer to the
		* full option '-t...', but doesn't check the '-', so
		* point just before the 't':
		*/
	       Process_Trace_Option ( cp-2 );
	       pass_option = FALSE;
	       break;

	     case 'u':
		if (*cp=='\0') {
		   /* Plain -u */
		   add_cray_args("-eI");
		   pass_option = FALSE;
		}
	       break;

	     case 'v':
	       if ( strcmp(cp,"ersion") == 0) {
		  /* -version, edg's -v */
		  /* Print out compiler version. */
		  fprintf(stderr, "mfef90 version %s\n", INCLUDE_STAMP);
		  fprintf(stderr, "Changeset: %s\n", cset_id);
		  fprintf(stderr, "Built by: %s@%s in %s\n", build_user, build_host, build_root);
		  fprintf(stderr, "Built on: %s\n", mfef90_compiler_build_date);
		  pass_option = FALSE;
	       }
	       break;

	     case 'w':
	       if (*cp=='\0') {
		  add_cray_args("-m4");
		  pass_option = FALSE;
		  Min_Error_Severity = ES_ERROR;
	       }  else if ( strncmp(cp, "off", 3) == 0) {
		  Cray_Woff(cp+3);
		  pass_option = FALSE;
	       } else if ( *cp=='2') { /* -w2 print warnings */
		  Min_Error_Severity = ES_WARNING;
	       }
	       else if ( *cp=='3') { /* -w3: exit 1 on warnings */
		  Min_Error_Severity = ES_ERROR;
	       }
	       pass_option = FALSE;
	       break;
	    } /* switch */
	    if (pass_option) {
	       add_cray_args(argv[i]);
	    }
	 }
      } else {
	 /* add to the non-switch list */
	 add_deferred_cray_args(argv[i]);
      }
   }

#ifdef KEY /* Bug 5061 */
   /* Fortran front end relies on wn_simp to fold certain expressions, so
    * don't allow Process_Command_Line_Group() above to turn it off */
   Enable_WN_Simp = TRUE;
#endif /* KEY Bug 5061 */

   /* Add the align switch */
   if (FE_align==8) {
      add_cray_args("-adalign");
   }

   /* Read the command line and pass it along to the Cray front-end */
   if (FE_command_line) {
      f = fopen(FE_command_line,"r");
      command_line_length = 0;
      if (f) {
	 while ( getc(f) != '\n' ) ++command_line_length;
	 fseek (f,0,SEEK_SET);
	 temp = (char *) malloc(command_line_length+2);
	 fgets(temp,command_line_length+1,f);
	 temp[command_line_length] = 0;
	 fclose(f);
	 add_cray_args("-CY");
	 add_cray_args(temp);
	 free(temp);
      }
   }
}   


/* ====================================================================
 * Entry point from Cray compiler
 * ====================================================================
 */

void sgi_cmd_line (INT *argc, char ***argv)
{
   INT          i;
   INT          len;
   char         msg_num[10];
   char        *chp;
   option_list *opt_list;


   /* Set up SGI error handling */

   save_argv = (char **) malloc((*argc) * sizeof(char *));
   save_argc = *argc;

   for(i=0; i < *argc; i++) {
      len = strlen((*argv)[i]);
      save_argv[i] = (char *) malloc((len + 1)*sizeof(char));
      strcpy(save_argv[i], (*argv)[i]);
   }

   Set_Error_Tables (Phases, host_errlist);
   Init_Error_Handler ( 100 );
   Set_Error_Line ( ERROR_LINE_UNKNOWN );
   Set_Error_File ( NULL );
   Set_Error_Phase ( "Front End Parse/Semantic");
   Handle_Signals();

   /* Set the language to be F90 */
   Language = LANG_F90;
   Preconfigure ();

   /* set this so that Cray will open the file for us */
   Process_Command_Line ( *argc, *argv);

   MEM_Initialize() ;
   MEM_POOL_Initialize(FE_Mempool,"FE_Mempool",TRUE);
   MEM_POOL_Push(FE_Mempool);

   Configure ();
   Configure_Source (NULL);
//   Configure_Target() ;
   IPA_Configure_Target ();

   /* Set the target name for MDupdate */
   if (mdtarget_file == NULL ) mdtarget_file = object_file_name;

   /* Add a few more arguments after the target configuration */
   if (Pointer_Size == 8) {
      add_cray_args("-spointer8");
   }

   if (FTN_Short_Circuit_On) {
      add_cray_args("-Oshortcircuit0");
   } else {
      add_cray_args("-Oshortcircuit1");
   }

   /* add the Matrix-multiply inline options */
   if (!Mv_Matmul_Inline_Set) {
     Mv_Matmul_Inline = (Opt_Level >= 3);
   } 
   if (!Matmul_Inline_Set) {
     Matmul_Inline = (Opt_Level >= 3);
   }
   if (Matmul_Inline) {
     add_cray_args("-Omatmul_inline");
   }
   if (Mv_Matmul_Inline) {
     add_cray_args("-Omv_matmul_inline");
   }

   /* Bounds checking */

   if (DEBUG_Subscript_Check) {
     add_cray_args("-Rb");
   }

   if (DEBUG_Fullwarn) {
     add_cray_args("-m2");
   }

		  /* Cray_Woff(cp+3); */

   if (Current_DEBUG->error_set) {
      opt_list = Current_DEBUG->error;

      while (opt_list != NULL) {
         chp = strtok(opt_list->val, ",");

         while(chp != NULL) {
            add_cray_args("-M");
            msg_num[0] = 'E';
            strcpy(&msg_num[1], chp);
            add_cray_args(msg_num);
            chp = strtok(NULL, ",");
         }
         opt_list = opt_list->next;
      } 
   } 

   if (Current_DEBUG->warning_set) {
      opt_list = Current_DEBUG->warning;

      while (opt_list != NULL) {
         chp = strtok(opt_list->val, ",");

         while(chp != NULL) {
            add_cray_args("-M");
            msg_num[0] = 'W';
            strcpy(&msg_num[1], chp);
            add_cray_args(msg_num);
            chp = strtok(NULL, ",");
         }
         opt_list = opt_list->next;
      } 
   } 

   if (enable_mp_processing) {

      if (! disable_old_mp) {
         add_cray_args("-ump");
      }

      if (! disable_open_mp) {
         add_cray_args("-uopen_mp");
      }
   }

   /* Add the deferred arguments */
   for (i = 0 ; i < num_deferred_cray_args; i++) {
      add_cray_args(deferred_argv_cray[i]);
   }
   /* Add a final NULL arg */
   add_cray_args(NULL);
   --num_cray_args;
   
   /* Clean up the mess */
   free(deferred_argv_cray);

   argc_cray = num_cray_args;
   *argc = argc_cray;
   *argv = argv_cray;
   
   if (Get_Trace(TP_IRB, 1) != 0) {
      fprintf(TFile,"Cray command line arguments:\n");
      for (i = 0; i < argc_cray; i++) {
	 fprintf(TFile,"Arg %d: |%s|\n",i,argv_cray[i]);
      }
      fprintf(TFile,"\n");
   }
   return;
}


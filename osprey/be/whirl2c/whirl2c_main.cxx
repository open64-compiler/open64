/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Dynamically set up the right LD_LIBRARY_PATH before exec'ing the backend
 * proper.
 */

#include <stdio.h>		    /* for fprintf () */
#include <stdlib.h>		    /* for getenv() */
#include <unistd.h>		    /* for execv() */
#include <string.h>		    /* for strcpy(), strerror() */
#include <limits.h>		    /* for PATH_MAX */
#include <errno.h>		    /* for errno */
#include <stdarg.h>
#include <cmplrs/rcodes.h>
#include "defs.h"

#if !defined(SHARED_BUILD)
/* no weak version, so need stub to compile (real version is in libwopt) */
#include "wn.h"
#include "opt_defs.h"
AUX_ID WN_aux (const WN*) { return 0; }
#endif


char path[PATH_MAX];
static const char *libpath[3] = 
{
#if defined (__MACH__) && defined(__APPLE__)
  "DYLD_LIBRARY_PATH",
#elif defined(_AIX)
  "LIBPATH",
#else 
"LD_LIBRARY_PATH",
#endif
 "LD_LIBRARYN32_PATH",
 "LD_LIBRARY64_PATH"
};

static const char * const errstring = "%s: can't allocate memory\n";
static const char *program_name;

/* Copied from driver/errors.c */
void
error(const char *format, ...)
{
        va_list args;
        va_start (args, format);
        fprintf(stderr, "%s ERROR:  ", program_name);
        vfprintf(stderr, format, args);
        fprintf(stderr, "\n");
        va_end (args);
}


static BOOL
Has_Extension (const char *name,  /* The filename to check */
	       const char *ext)   /* The extension to look for */
{
  INT16 nlen = strlen(name);
  INT16 elen = strlen(ext);

  /* If ext is longer than name, no chance: */
  if ( elen > nlen ) return FALSE;

  /* Otherwise compare the tail of name to ext: */
  return ( strcmp ( &name[nlen-elen], ext ) == 0 );
} /* Has_Extension */


static void
Usage (const char *progname)
{
   fprintf(stderr, 
	   "USAGE: in EBNF notation, where '|' indicates choice and '['\n"
	   "indicates an optional item:\n"
	   "\n"
	   "\t%s [-CLIST:<opts>] [-TARG:<t>] [-TENV:<e>] <inp_files>\n"
	   "\n"
	   "\t<inp_files> ::= [-fB,<Whirl_File_Name>] <File_Name>\n"
	   "\t<opts> ::= <single_opt>[:<opts>]\n"
	   "\n"
	   "We recommend always using the common option -TARG:abi=[32|64].\n"
	   "\n"
	   "The <File_Name> is a mandatory command-line argument, which may\n"
	   "denote either a (Fortran) source filename or a WHIRL file.\n"
	   "In the abscense of a -fB option, the <Whirl_File_Name> will be\n"
	   "derived from the <File_Name>\n" 
	   "\n",
	   progname);
   
   fprintf(stderr,
	   "Each -CLIST:<single_opt> is described below:\n"
	   "\n"
	   "-CLIST:show\n"
	   "\tIndicate the input/output file-names to stderr.\n"
	   "-CLIST:linelength=<n>\n"
	   "\tSpecifies an upper limit on the number of characters we allow\n"
           "\ton each line in the output file\n"
	   "-CLIST:emit_adims\n"
	   "\tComment multi-dimensional array indexing expressions to\n"
	   "\tindicate which subexpression denotes which dimension.\n"
	   "-CLIST:emit_pfetch\n"
	   "\tEmit comments to indicate prefetch instructions.\n"
	   "-CLIST:emit_regions\n"
	   "\tEmit all regions, whether user defined or compiler generated.\n"
	   "\tThe default is to only emit user-defined regions.\n"
	   "-CLIST:emit_linedirs\n"
	   "\tEmit #line directives to map the generated statements back to\n"
	   "\tthe corresponding original source statements.\n"
	   "-CLIST:emit_nested_pu\n"
	   "\tEmit code for PUs nested within other PUs.  Currently, the\n"
	   "\tsymbol-table context will not be correctly set up for this and\n"
	   "\tthe nested PU will be emitted immediately after the parent PU.\n"
	   "\tNote that this will also lower MP constructs.\n"
	   "-CLIST:emit_frequency\n"
	   "\tEmit feedback frequency numbers for each statement.  The\n"
	   "\tfrequency information will be emitted using the comment\n"
	   "\tnotation, and will apply to statement up till the previous\n"
	   "\tfrequency information seen.\n"
	   "-CLIST:ftn\n"
	   "\tThe intermediate is for a Fortran source program; intrinsics\n"
	   "\tand IO-statements must be lowered for translation to C.\n"
	   "-CLIST:src_file=<Src_File_Name>\n"
	   "\tThe name of the original source program.  When not given,\n"
	   "\tthe <Src_File_Name> is derived from the <File_Name>.\n"
	   "-CLIST:doth_file=<H_File_Name>\n"
	   "\tThe file into which file-level declarations will be emitted.\n"
	   "\tWhen not given, <H_File_Name> is derived from <Src_File_Name>.\n"
	   "-CLIST:dotc_file=<C_File_Name>\n"
	   "\tThe file into which program units will be emitted.  When\n"
	   "\tnot given, <C_File_Name> is derived from <Src_File_Name>.\n"
	   "-CLIST:loc_file=<Loc_File_Name>\n"
	   "\tThe file for emission of a mapping from positions in the\n"
	   "\tsource to corresponding positions in the original source\n"
	   "\tfile.  Without this option, no such file is created.\n"
	   "\n");
   fprintf(stderr,
	   "Compile the generated <C_File_Name> with \"-dollar -lm\".  When\n"
	   "-CLIST:ftn, then compile the generated <C_File_Name> with\n"
           "\"-D_FORTRAN2C -dollar -lftn\" and possibly other \"-l\" options\n"
	   "to account for libraries referenced in the source.\n"
	   "\n");
} /* Usage */


int
main (INT argc,                   /* Number of command line arguments */
      const char *const argv[],   /* Array of command line arguments */
      const char *const envp[])   /* Array of environment pointers */
{
   /* Take the current value for the environment variable LD_LIBRARY_PATH,
    * then add in the path where this executable was found, assuming any
    * necessary DSOs (be.so and whirl2c.so) are found in the same
    * directory.  Then execute "whirl2c_be", also assumed to be
    * in the directory where this executable resides.
    */
    register char      **new_argv;
    register char       *p;
    register const char *env;
    register INT         i, len;
    register INT         argidx;
    register BOOL        dash_fB_option = FALSE; /* Any -fB option? */
    char                *newlibpath[3];
    char                *TOOLRT;  // TOOTROOT 

    program_name = argv[0];

    if (argc == 1)
    {
       Usage(argv[0]);
       exit(RC_NORECOVER_USER_ERROR);
    }
#if defined(TARG_IA64)    
    TOOLRT = getenv("TOOLROOT");
    strcpy (path, TOOLRT);
    strcat (path, "/bin");
#else
    strcpy (path, argv[0]);
#endif

    if (p = strrchr(path, '/'))
	p[0] = 0;
    else
	strcpy (path, ".");

    for (i = 0; i<3; i++)
    {
       len = strlen (path) + 1;
       len += strlen (libpath[i]) + 1;    /* env. variable name plus '=' */

       env = getenv (libpath[i]);

       if (env) {
	  len += strlen (env) + 1;    /* old path plus ':' */

	  newlibpath[i] = (char *) malloc (len);
	  if (newlibpath[i] == 0) {
	     fprintf (stderr, errstring, argv[0]);
	     exit(RC_NORECOVER_USER_ERROR);
	  }

	  sprintf (newlibpath[i], "%s=%s:%s", libpath[i], env, path);
       } else {
	  newlibpath[i] = (char *) malloc (len);
	  if (newlibpath[i] == 0) {
	     fprintf (stderr, errstring, argv[0]);
	     exit(RC_NORECOVER_USER_ERROR);
	  }

	  sprintf (newlibpath[i], "%s=%s", libpath[i], path);
       }
    } /* For each libpath kind */

    /* Copy the argument list into a new list of strings, with a spare
     * element for a missing -fB option.
     */
    new_argv = (char **)malloc((argc+3)*sizeof(char *));
    for (argidx = 0; argidx < argc; argidx++)
    {
       new_argv[argidx] = (char *)malloc(strlen(argv[argidx]) + 1);
       new_argv[argidx] = strcpy(new_argv[argidx], argv[argidx]);
       if (new_argv[argidx][0] == '-' &&
	   new_argv[argidx][1] == 'f' &&
	   new_argv[argidx][2] == 'B')
       {
	  dash_fB_option = TRUE;
       }
    }

    if (!dash_fB_option)
    {
       /* Create a "-fB" option, provided the file-argument (only argument
	* not preceded by a '-') represents the WHIRL file if suffixed by
	* ".B", ".I", ".N" or ".o".
	*/
       argidx = argc-1;
       while (argidx > 0)
       {
	  if (new_argv[argidx][0] != '-' && /* A file argument */
	      (Has_Extension(new_argv[argidx], ".B") ||
	       Has_Extension(new_argv[argidx], ".I") ||
	       Has_Extension(new_argv[argidx], ".N") ||
	       Has_Extension(new_argv[argidx], ".O") ||
	       Has_Extension(new_argv[argidx], ".o")))
	  {
	     /* A file argument representing the WHIRL input file.  We need
	      * to change this around a little bit.  Put this filename under
	      * a "-fB,filename" option and add a new filename with the
	      * suffix substituted by ".c".
	      */
	     dash_fB_option = TRUE;
	     new_argv[argc] = (char *)malloc(strlen(new_argv[argidx]) + 5);
	     (void)strcpy(new_argv[argc], "-fB,");
	     (void)strcpy(&new_argv[argc][4], new_argv[argidx]);
	     argc++;
	  
	     new_argv[argidx][strlen(new_argv[argidx])-1] = 'c';
	     argidx = 1; /* We are done! */
	  }
	  argidx--;
       } /*while*/
    } /*if (!dash_fB_option)*/

#if defined(__MACH__) && defined(__APPLE__) 
    //force -run-w2c on all OSXes regardless of version
    new_argv[argc] = (char *)malloc(20);
    strcpy(new_argv[argc],"-run-w2c");
    new_argv[argc+1] = NULL;
# else
    new_argv[argc] = NULL;
#endif     

#if defined(TARG_IA64)
    strcat (newlibpath[0],"/lib/gcc-lib/ia64-open64-linux/4.0/");
#endif

    for (i = 0; i<3; i++)
       putenv (newlibpath[i]);

    strcat (path, "/whirl2c_be");


#if defined(__MACH__) && defined(__APPLE__)
    //bug1362: OSX 10.3 needs to have a real absolute path for
    // execv(), otherwise subsequent dlopens may fail
    char abspath[PATH_MAX];
    if(!realpath(path,abspath)) perror("realpath");
    new_argv[0] = abspath;
    strcpy(path, abspath);
#endif

    execv (path, new_argv);
    error("%s: fail to execute %s: %s.\n", argv[0], new_argv[0], strerror(errno));
    exit(RC_SYSTEM_ERROR);
} /* main */



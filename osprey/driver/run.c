/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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


#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>

#if !defined(_WIN32)
 #include <sys/wait.h>
 #include <sys/times.h>
#if !defined(__APPLE__)
 #include <sys/procfs.h>
#endif
 #include <unistd.h>
 /* cygwin procfs.h defines _WIN32, so use __MINGW32__ after this */
#else /* _WIN32 */
 #include <windows.h>
 #include <process.h> /* spawn */
 #include <time.h>    /* time, difftime */
#endif /* _WIN32 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <malloc.h>
#include <sys/param.h>
#include <limits.h>
#include <alloca.h>
#include <cmplrs/rcodes.h>
#include "run.h"
#include "string_utils.h"
#include "errors.h"
#include "file_names.h"
#include "phases.h"
#include "opt_actions.h"
#include "option_seen.h"
#include "file_utils.h"
#include "pathscale_defs.h"
#include "option_names.h"
#include "lib_phase_dir.h"

boolean show_flag = FALSE;
boolean v_flag = FALSE;
boolean show_but_not_run = FALSE;
boolean execute_flag = TRUE;
boolean time_flag = FALSE;
boolean prelink_flag = TRUE;
boolean quiet_flag = TRUE;
boolean run_m4 = FALSE;
static boolean ran_twice = FALSE;
static int f90_fe_status = RC_OKAY;
static char *f90_fe_name = NULL; 

static void init_time (void);
static void print_time (char *phase);
static void my_psema(void);
static void my_vsema(void);

extern boolean add_heap_limit;
extern int heap_limit;
extern int hugepage_attr;

#define LOGFILE "/var/log/messages"

#if !defined(__MINGW32__)
static void my_execv(const char *name, char *const argv[])
{
    int len = strlen(name);
    int passthru = len > 4 && name[len - 4] == '/' &&
	(strcmp(name + len - 3, "gcc") == 0 ||
	 strcmp(name + len - 3, "g++") == 0);
    
    if (show_but_not_run) {
	int i;

	fprintf(stderr, "\"%s\" ", name);
	for (i = 1; argv[i] != NULL; i++)
	    fprintf(stderr, "\"%s\" ", argv[i]);
	fputc('\n', stderr);
	if (!passthru)
	    do_exit (0);
    }
			
    execv(name, argv);
    error("cannot exec %s: %m", name);
    cleanup ();
    do_exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
}
#endif

/* exec another program, putting result in output.
 * This is simple version of full run_phase. */
void
run_simple_program (char *name, char **argv, char *output)
{
#if defined(__MINGW32__)
	internal_error("run_simple_program not supported");
#else
	int forkpid;
	int fdout;
	int waitpid;
	int waitstatus;
	int termsig;

	/* fork a process */
	forkpid = fork();
	if (forkpid == -1) {
		error("no more processes");
		cleanup ();
		do_exit (RC_SYSTEM_ERROR);
		/* NOTREACHED */
	}

	if (forkpid == 0) {
		/* child */
		if ((fdout = creat (output, 0666)) == -1) {
			error ("cannot create output file %s", output);
			cleanup ();
			do_exit (RC_SYSTEM_ERROR);
			/* NOTREACHED */
		}
	    	dup2 (fdout, fileno(stdout));

		my_execv(name, argv);
	} else {
		/* parent */
		int procid;	/* id of the /proc file */
		while ((waitpid = wait (&waitstatus)) != forkpid) {
			if (waitpid == -1) {
				error("bad return from wait");
				cleanup();
				do_exit(RC_SYSTEM_ERROR);
				/* NOTREACHED */
			}
		}
		if (WIFSTOPPED(waitstatus)) {
			termsig = WSTOPSIG(waitstatus);
			error("STOPPED signal received from %s", name);
			cleanup();
			do_exit(RC_SYSTEM_ERROR);
			/* NOTREACHED */
		} else if (WIFEXITED(waitstatus)) {
		        int status = WEXITSTATUS(waitstatus);
			if (status != RC_OKAY) {
				/* internal error */
				/* most internal errors use exit code of 1 */
				internal_error("%s returned non-zero status %d",
					name, status);
			} 
			return;
		} else if(WIFSIGNALED(waitstatus)){
			termsig = WTERMSIG(waitstatus);
			switch (termsig) {
			case SIGHUP:
			case SIGINT:
			case SIGQUIT:
			case SIGKILL:
			case SIGTERM:
				error("%s died due to signal %d", name, termsig);
				break;
			default:
				internal_error("%s died due to signal %d",
					       name, termsig);
				break;
			}
			if(WCOREDUMP(waitstatus)) {
				error("core dumped");
			}
			if (termsig == SIGKILL) {
				error("Probably caused by running out of swap space -- check %s", LOGFILE);
			}
			cleanup();
			do_exit(RC_SYSTEM_ERROR);
		} else {
			/* cannot happen, I think! */
			internal_error("driver exec'ing is confused");
			return;
		}
	}
#endif /* __MINGW32__ */
}

static void my_putenv(const char *name, const char *fmt, ...)
    __attribute__((format (printf, 2, 3)));
    
static void my_putenv(const char *name, const char *fmt, ...)
{
    va_list ap;
    char *rhs, *env;
    int len;
    va_start(ap, fmt);

    if (vasprintf(&rhs, fmt, ap) == -1) {
	internal_error("cannot allocate memory");
	do_exit(RC_SYSTEM_ERROR);
    }
	
    if (show_but_not_run)
	fprintf(stderr, "%s=\"%s\" ", name, rhs);

    // This looks like a memory leak, but it's not interesting, because
    // we're either going to call exec() or do_exit() soon after we're done.

    if (asprintf(&env, "%s=%s", name, rhs) == -1) {
	internal_error("cannot allocate memory");
	do_exit(RC_SYSTEM_ERROR);
    }

    putenv(env);
	
    va_end(ap);
}

char *
get_binutils_lib_path(void)
{
	#ifdef PSC_TO_OPEN64
	static const char *binutils_library_path = "../i686-pc-linux-gnu/" OPEN64_TARGET "/lib";
	#endif
	char *my_path;
	
	asprintf(&my_path, "%s/%s", get_executable_dir(),
		 binutils_library_path);
	return my_path;
}


void
run_phase (phases_t phase, char *name, string_list_t *args)
{
	char **argv;
	int argc;
	string_item_t *p;
	char *output = NULL;
	char *input = NULL;
	boolean save_stderr = FALSE;
	int fdin, fdout;
	int forkpid;
	int waitpid;
	int waitstatus;
	int termsig;
	int	num_maps;
	char *rld_path;
	struct stat stat_buf;
	const boolean uses_message_system = 
			(phase == P_f90_fe || phase == P_f90_cpp ||
			 phase == P_cppf90_fe);

        if (((phase == P_be) || (phase == P_ipl))) {
            if (add_heap_limit) {
                char buf[100];
                char * str;
                sprintf(&buf[0],"%d", heap_limit);
                str = concat_strings("-OPT:hugepage_heap_limit=", buf);
                sprintf(&buf[0],"%d", hugepage_attr);
                str = concat_strings(str, 
                                     concat_strings(" -OPT:hugepage_attr=", buf));
                add_string(args, str);
            }

            if (oscale == TRUE)
                add_string(args, "-OPT:scale=ON");
        }

	if (show_flag) {
		/* echo the command */
		fprintf(stderr, "%s ", name);
		print_string_list(stderr, args);
	}

	/* When using -v (and not -show), do not echo out the gcc call
	   that does the link phase.  libtool uses the -v option and greps
	   for lines with -L to determine the link method when building
	   shared libraries.  Echoing both the gcc call and the collect2/ld
	   call that gcc generates confuses libtool.  */

	if (v_flag && !show_flag && phase != P_ld && phase != P_ldplus) {
		fprintf(stderr, "%s ", name);
		print_string_list(stderr, args);
	}

	if (!execute_flag) return;

	if (time_flag) init_time();

	/* copy arg_list to argv format that exec wants */
	argc = 1;
	for (p = args->head; p != NULL; p=p->next) {
		//bug# 581, bug #932, bug #1049
		if (p->name == NULL) 
                  continue;
		argc++;
	}
	argv = (char **) malloc((argc+1)*sizeof(char*));
	argv[0] = name;
	for (argc = 1, p = args->head; p != NULL; argc++, p=p->next) {
		//bug# 581, bug #932
		if (p->name[0] == '\0') {
		  argc--;
                  continue;
		}
		/* don't put redirection in arg list */
		if (strcmp(p->name, "<") == 0) {
			/* has input file */
			input = p->next->name;
			break;
		} else if (strcmp(p->name, ">") == 0) {
			/* has output file */
			output = p->next->name;
			break;
		} else if (strcmp(p->name, ">&") == 0) {
			/* has error output file */
			output = p->next->name;
			save_stderr = TRUE;
			break;
		}
		argv[argc] = p->name;
	}
	argv[argc] = NULL;

#if !defined(__MINGW32__)
	/* fork a process */
	forkpid = fork();
	if (forkpid == -1) {
		error("no more processes");
		cleanup ();
		do_exit (RC_SYSTEM_ERROR);
		/* NOTREACHED */
	}

	if (forkpid == 0) {
		char *my_path, *l_path, *l32_path, *nls_path, *env_path;
		
		/* child */
		/* if we want memory stats, we have to wait for
		   parent to connect to our /proc */
		if (input != NULL) {
			if ((fdin = open (input, O_RDONLY)) == -1) {
				error ("cannot open input file %s", input);
				cleanup ();
				do_exit (RC_SYSTEM_ERROR);
				/* NOTREACHED */
			}
	    		dup2 (fdin, fileno(stdin));
		}
		if (output != NULL) {
			if ((fdout = creat (output, 0666)) == -1) {
				error ("cannot create output file %s", output);
				cleanup ();
				do_exit (RC_SYSTEM_ERROR);
				/* NOTREACHED */
			}
			if (save_stderr) {
	    			dup2 (fdout, fileno(stderr));
			} else {
	    			dup2 (fdout, fileno(stdout));
			}
		} 

		my_path = get_binutils_lib_path();
		rld_path = get_phase_ld_library_path (phase);
		
		if (rld_path != 0)
			asprintf(&my_path, "%s:%s", my_path, rld_path);
		
		l_path = l32_path = my_path;
		
		if (ld_library_path)
			asprintf(&l_path, "%s:%s", my_path, ld_library_path);

		if (ld_libraryn32_path)
			asprintf(&l32_path, "%s:%s", my_path,
				 ld_libraryn32_path);

		my_putenv("LD_LIBRARY_PATH", l_path);
		my_putenv("LD_LIBRARYN32_PATH", l32_path);
		
		// Set up NLSPATH, for the Fortran front end.

		nls_path = getenv("NLSPATH");
		env_path = get_phase_dir(P_f90_fe);

		if (nls_path) {
		    my_putenv("NLSPATH", "%s:%s/%%N.cat", nls_path, env_path);
		} else {
		    my_putenv("NLSPATH", "%s/%%N.cat", env_path);
		}

		if (uses_message_system && getenv("ORIG_CMD_NAME") == NULL)
		   my_putenv("ORIG_CMD_NAME", "%s", program_name);

		if (phase == P_f90_fe) {
		   char *root;
		   char *modulepath;
		   int len;
		   char *new_env;
		   char *env_name = "FORTRAN_SYSTEM_MODULES=";
		   char *env_val = "/usr/lib/f90modules";
		   root = getenv("TOOLROOT");
		   if (root != NULL) {
		      len = strlen(env_val) + strlen(root) +3 + strlen(env_val);
		      new_env = alloca(len);
		      sprintf(new_env,"%s/%s:%s",root,env_val,env_val);
		      env_val = new_env;
		   }
		   modulepath = string_copy(getenv("FORTRAN_SYSTEM_MODULES"));
		   if (modulepath != NULL) {
		      /* Append env_val to FORTRAN_SYSTEM_MODULES */
		      if (modulepath[strlen(modulepath)-1] == ':') {
			 /* Just append env_val */
			 len = strlen(modulepath) + strlen(env_val) + 1;
			 new_env = alloca(len);
			 sprintf(new_env,"%s%s",modulepath,env_val);
		      } else {
			 /* append :env_val */
			 len = strlen(modulepath) + strlen(env_val) + 2;
			 new_env = alloca(len);
			 sprintf(new_env,"%s:%s",modulepath,env_val);
		      }
		      env_val = new_env;
		   }
		   
		   my_putenv ("FORTRAN_SYSTEM_MODULES", "%s", env_val);
		}

		/* need to setenv COMPILER_PATH for collect to find ld */

                // gcc will invoke the cc1 in the COMPILER_PATH directory,
		// which is not what we want when we invoke gcc for
		// preprocessing.  Bug 10164.
		if (!is_matching_phase(get_phase_mask(P_any_cpp), phase))
		my_putenv ("COMPILER_PATH", "%s", get_phase_dir(P_collect));

		/* Tell IPA where to find the driver. */
		// Fix the bug "*/cc-3.0: No such file or directory"
		// actually, on IA64, the corresponding path is from $TOOLROOT
		//
		my_putenv ("COMPILER_BIN", "%s/" BINPATH "/" OPEN64_NAME_PREFIX "cc", global_toolroot);
		my_execv(name, argv);
	} else {
		/* parent */
		int procid;	/* id of the /proc file */
		while ((waitpid = wait (&waitstatus)) != forkpid) {
			if (waitpid == -1) {
				error("bad return from wait");
				cleanup();
				do_exit(RC_SYSTEM_ERROR);
				/* NOTREACHED */
			}
		}
		if (time_flag) print_time(name);

		if (WIFSTOPPED(waitstatus)) {
			termsig = WSTOPSIG(waitstatus);
			error("STOPPED signal received from %s", name);
			cleanup();
			do_exit(RC_SYSTEM_ERROR);
			/* NOTREACHED */
		} else if (WIFEXITED(waitstatus)) {
		        int status = WEXITSTATUS(waitstatus);
			extern int inline_t;
			boolean internal_err = FALSE;
			boolean user_err = FALSE;
		
			if (phase == P_prof) {
                           /* Make sure the .cfb files were created before
                              changing the STATUS to OKAY */
                           if (prof_file != NULL) {
                              if (!(stat(fb_file, &stat_buf) != 0 && errno == ENOENT))
                                  status = RC_OKAY;
                           } else {
			      internal_error("No count file was specified for a prof run");
			      perror(program_name);
                           }
                        }

			if (phase == P_f90_fe && keep_listing) {
			    char *cif_file;
			    cif_file = construct_given_name(
					  drop_path(source_file), "T", TRUE);
                            if (!(stat(cif_file, &stat_buf) != 0 && errno == ENOENT))
			       f90_fe_status = status;
			       f90_fe_name = string_copy(name);

			       /* Change the status to OKAY so that we can 
				* execute the lister on the cif_file; we will
				* take appropriate action on this status once 
				* the lister has finished executing. See below.
				*/

			       status = RC_OKAY;
                        }

			if (phase == P_lister) {
			   if (status == RC_OKAY && f90_fe_status != RC_OKAY) {

			      /* We had encountered an error in the F90_fe phase
			       * but we ignored it so that we could execute the
			       * lister on the cif file; we need to switch the
			       * status to the status we received from F90_fe
			       * and use the name of the F90_fe_phase, so that
			       * we can issue a correct error message.
			       */

			       status = f90_fe_status;
			       name = string_copy(f90_fe_name);

			       /* Reset f90_fe_status to OKAY for any further
				* compilations on other source files.
				*/

			       f90_fe_status = RC_OKAY;
                           }
                        }

			switch (status) {
			case RC_OKAY:
				// If the command line has explicit inline
				// setting, follow it; else follow the
				// front-end request.  Bug 11325.
				if (inline_t != UNDEFINED) {
				  run_inline = inline_t;
				  break;
				}

				// bug 10215
				if (gnu_major_version == 4) {
				  if (is_matching_phase(get_phase_mask(phase),
							P_wgen)) {
				    run_inline = FALSE;
				  }
				  break;
				}
				if (inline_t == UNDEFINED
				    && is_matching_phase(
					get_phase_mask(phase), P_any_fe) )
				{
					run_inline = FALSE;	// bug 11325
				}
				break;
			case RC_NEED_INLINER:
                                // If the command line has explicit inline
				// setting, follow it; else follow the
				// front-end request.  Bug 11325.
				if (inline_t != UNDEFINED) {
				  run_inline = inline_t;
				  break;
				}

				// bug 10215
				if (gnu_major_version == 4) {
				  if (is_matching_phase(get_phase_mask(phase),
							P_wgen)) {
				    run_inline = TRUE;
				  }
				  break;
				}
				if (inline_t == UNDEFINED
				    && is_matching_phase(
					get_phase_mask(phase), P_any_fe) )
				{
					run_inline = TRUE;	// bug 11325
				}
				/* completed successfully */
				break;
				
			case RC_USER_ERROR:
			case RC_NORECOVER_USER_ERROR:
			case RC_SYSTEM_ERROR:
			case RC_GCC_ERROR:
				user_err = TRUE;
				break;

			case RC_OVERFLOW_ERROR:
				if (!ran_twice && phase == P_be) {
					/* try recompiling with larger limits */
					ran_twice = TRUE;
					add_string (args, "-TENV:long_eh_offsets");
					add_string (args, "-TENV:large_stack");
					run_phase (phase, name, args);
					return;
				}
				internal_err = TRUE;
				break;
			case RC_GCC_INTERNAL_ERROR:
			case RC_INTERNAL_ERROR:
				internal_err = TRUE;
				break;
			default:
				internal_err = TRUE;
				break;
			} 
			if (internal_err) {
				if (phase == P_ld || phase == P_ldplus ||
				    phase == P_gas ||	// bug 4846
				    phase == P_f_coco ||	// bug 9058
				    phase == P_spin_cc1 ||
				    phase == P_spin_cc1plus ||
				    status == RC_GCC_INTERNAL_ERROR ||  //bug 9637
				    phase == P_gcpp || phase == P_gcpp_plus) {
					if (phase == P_gas ||
					    status == RC_GCC_INTERNAL_ERROR) {
						internal_error_occurred = 1;
					}
					log_error("%s returned non-zero status %d",
						  name, status);
					nomsg_error(status);
				} else {
					internal_error("%s returned non-zero status %d",
						       name, status);
				}
			}
			else if (user_err) {
				/* assume phase will print diagnostics */
				if (phase == P_c_gfe || phase == P_cplus_gfe
				    || phase == P_wgen
				    || phase == P_spin_cc1
				    || phase == P_spin_cc1plus
				   ) {
					nomsg_error(RC_INTERNAL_ERROR);
				}
				else if (!(show_flag || v_flag) || save_stderr) {
					nomsg_error(RC_USER_ERROR);
				} else {
					error("%s returned non-zero status %d",
						name, status);
				}
			}
			ran_twice = FALSE;
			return;
		} else if(WIFSIGNALED(waitstatus)){
			termsig = WTERMSIG(waitstatus);
			switch (termsig) {
			case SIGHUP:
			case SIGINT:
			case SIGQUIT:
			case SIGKILL:
			case SIGTERM:
				error("%s died due to signal %d", name, termsig);
				break;
			default:
				internal_error("%s died due to signal %d",
					       name, termsig);
				break;
			}
			if(WCOREDUMP(waitstatus)) {
				error("core dumped");
			}
			if (termsig == SIGKILL) {
				error("Probably caused by running out of swap space -- check %s", LOGFILE);
			}
			cleanup();
			do_exit(RC_SYSTEM_ERROR);
		} else {
			/* cannot happen, I think! */
			internal_error("driver exec'ing is confused");
			return;
		}
	}

#else /* __MINGW32__ */

  if (input != NULL) {
    if ((fdin = open (input, O_RDONLY)) == -1) {
      error ("cannot open input file %s", input);
      cleanup ();
      exit (RC_SYSTEM_ERROR);
      /* NOTREACHED */
    }
    dup2 (fdin, fileno(stdin));
  }
  if (output != NULL) {
    if ((fdout = creat (output, 0666)) == -1) {
      error ("cannot create output file %s", output);
      cleanup ();
      exit (RC_SYSTEM_ERROR);
                                /* NOTREACHED */
    }
    if (save_stderr) {
      dup2 (fdout, fileno(stderr));
    } else {
      dup2 (fdout, fileno(stdout));
    }
  }

  rld_path = get_phase_ld_library_path (phase);

  if (rld_path != 0) {
    string new_env;
    string env_name = "LD_LIBRARY_PATH";
    int len = strlen (env_name) + strlen(rld_path) + 2;
    if (ld_library_path) {
      len += strlen (ld_library_path) + 1;
      new_env = alloca (len);
      sprintf (new_env, "%s=%s:%s", env_name, rld_path,
               ld_library_path);
    } else {
      new_env = alloca (len);
      sprintf (new_env, "%s=%s", env_name, rld_path);
    }
    putenv (new_env);
  }

{
  unsigned int total_bytes = 0;
  int i;
  char *argv_str, *new_a;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  DWORD exit_code;
  int spawnRet;

  ZeroMemory( &si, sizeof(si) );
  si.cb = sizeof(si);
  ZeroMemory( &pi, sizeof(pi) );

  /* Calculate a bound on the length of the overall command line.
     Backslashes and quotes may need to be escaped, doubling the length
     of each argument in the worst case.  Each argument may need to be
     surrounded by quotes and will be followed by either a space or nil
     character, so we add 3 bytes overhead for each argument.  */
  for (i = 0; i < argc; i++)
    total_bytes += (2 * strlen (argv[i])) + 3;
  argv_str = malloc (total_bytes);

  /* The following argument quoting code was derived from Cygwin.
     Copyright 1996, 1997, 1998, 1999, 2000 Cygnus Solutions.

     This program is free software; you can redistribute it and/or modify it
     under the terms of the GNU General Public License (GPL) as published by
     the Free Software Foundation; either version 2 of the License, or (at
     your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
     USA.  */

  new_a = argv_str;
  for (i = 0; i < argc; i++) {
    int nback;
    char *p = NULL;
    const char *a = argv[i];
    int len = strlen (a);
    if (len != 0 && !strpbrk (a, " \t\n\r\"")) {
      memcpy (new_a, a, len);
      new_a += len;
    } else {
      *new_a++ = '"';
      /* Handle embedded special characters " and \.
         A " is always preceded by a \.
         A \ is not special unless it precedes a ".  If it does,
         then all preceding \'s must be doubled to avoid having
         the Windows command line parser interpret the \ as quoting
         the ".  This rule applies to a string of \'s before the end
         of the string, since cygwin/windows uses a " to delimit the
         argument. */
      for (; (p = strpbrk (a, "\"\\")); a = ++p) {
        memcpy (new_a, a, p - a);
        new_a += (p - a);
        /* Find length of string of backslashes */
        nback = strspn (p, "\\");
        if (nback == 0) {
          /* No backslashes, so it must be a ".
             The " has to be protected with a backslash. */
          *new_a++ = '\\';
          *new_a++ = '"';
        } else {
          /* Add the run of backslashes */
          memcpy (new_a, p, nback);
          new_a += nback;
          /* Double up all of the preceding backslashes if they precede
             a quote or EOS. */
          if (!p[nback] || p[nback] == '"') {
            memcpy (new_a, p, nback);
            new_a += nback;
          }
          /* Point to last backslash */
          p += nback - 1;
        }
      }
      if (*a) {
        len = strlen (a);
        memcpy (new_a, a, len);
        new_a += len;
      }
      *new_a++ = '"';
    }
    if (i == argc - 1)
      *new_a++ = '\0';
    else
      *new_a++ = ' ';
  }

  spawnRet = CreateProcess(NULL, argv_str, NULL, NULL,
                           TRUE, 0, NULL, NULL, &si, &pi);
 if (time_flag) print_time(name);
 if (spawnRet == 0) {
   // could check errno here, but haven't done it yet.
   switch(GetLastError())
     {
     case E2BIG:
       error("Spawn:Too many arguments\n");
       break;
     case EINVAL:
       error("Spawn:Invalid mode\n");
       break;
     case ENOENT:
       error("Spawn: File/Path not found\n");
       break;
     case ENOEXEC:
       error("Spawn: Bad executable file\n");
       break;
     case ENOMEM:
       error("Spawn: Not enough memory\n");
       break;
     default:
       error("Could not create process %s. Windows error code %d\n", argv_str, GetLastError());
     }
   //error("no more processes");
   cleanup ();
   exit (RC_SYSTEM_ERROR);
   /* NOTREACHED */
 }
 WaitForSingleObject(pi.hProcess, INFINITE);
 GetExitCodeProcess(pi.hProcess, &exit_code);
 CloseHandle(pi.hProcess);
 CloseHandle(pi.hThread);

 if (exit_code == 0 || exit_code < -1) {
   // normal termination. exit_code should never be < -1
   int status = exit_code;
   extern int inline_t;
   boolean_t internal_err = FALSE;
   boolean_t user_err = FALSE;

   switch (status) {

   case RC_OKAY:
     if (inline_t == UNDEFINED &&
          is_matching_phase(get_phase_mask(phase), P_any_fe))
        inline_t = FALSE;
      break;

    case RC_NEED_INLINER:
      if (inline_t == UNDEFINED &&
          is_matching_phase(get_phase_mask(phase), P_any_fe))
        inline_t = TRUE;
        /* completed successfully */
      break;

    case RC_USER_ERROR:
    case RC_NORECOVER_USER_ERROR:
    case RC_SYSTEM_ERROR:
    case RC_GCC_ERROR:
      user_err = TRUE;
      break;

    case RC_OVERFLOW_ERROR:
      if (!ran_twice && phase == P_be) {
        /* try recompiling with larger limits */
        ran_twice = TRUE;
        add_string (args, "-TENV:long_eh_offsets");
        add_string (args, "-TENV:large_stack");
        run_phase (phase, name, args);
        return;
      }
      internal_err = TRUE;
      break;

    case RC_INTERNAL_ERROR:
      if (phase == P_ld || phase == P_ldplus)
        /* gcc/ld returns 1 for undefined */
        user_err = TRUE;
      else
        internal_err = TRUE;
      break;

    default:
      if (phase == P_c_gfe || phase == P_cplus_gfe)
        user_err = TRUE;
      else
        internal_err = TRUE;
      break;
    }

    if (internal_err) {
      error("%s returned non-zero status %d", name, status);
    }
    else if (user_err) {
      /* assume phase will print diagnostics */
      if (!(show_flag || v_flag) || save_stderr)
	nomsg_error(RC_USER_ERROR);
      else
        error("%s returned non-zero status %d", name, status);
    }
    ran_twice = FALSE;
    return;
  }
  else {
    /* Abnormal exit. It's assumed that the spawned process
     * hasn't called exit with exit code > 0
     */
    error("error from spawn");
    cleanup ();
    exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
  }
}

#endif /* __MINGW32__ */
}

/*
 * Handler () is used for catching signals.
 */
void
handler (int sig)
{
#ifndef __MINGW32__
	error("signal %s caught, stop processing", strsignal(sig));
#else
	error("signal %d caught, stop processing", sig);
#endif
	cleanup ();
	do_exit (RC_SYSTEM_ERROR);
}

/* set signal handler */
void
catch_signals (void)
{
    /* modelled after Handle_Signals in common/util/errors.c */
#ifndef __MINGW32__
    if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
        signal (SIGHUP,  handler);
    if (signal (SIGQUIT, SIG_IGN) != SIG_IGN)
        signal (SIGQUIT,  handler);
    if (signal (SIGBUS, SIG_IGN) != SIG_IGN)
        signal (SIGBUS,  handler);
    if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
        signal (SIGPIPE,  handler);
    if (signal (SIGTRAP, SIG_IGN) != SIG_IGN)
        signal (SIGTRAP,  handler);
#endif
    if (signal (SIGILL, SIG_IGN) != SIG_IGN)
        signal (SIGILL,  handler);
    if (signal (SIGINT, SIG_IGN) != SIG_IGN)
        signal (SIGINT,  handler);
    if (signal (SIGABRT, SIG_IGN) != SIG_IGN)
        signal (SIGABRT,  handler);
    if (signal (SIGFPE, SIG_IGN) != SIG_IGN)
        signal (SIGFPE,  handler);
    if (signal (SIGSEGV, SIG_IGN) != SIG_IGN)
        signal (SIGSEGV,  handler);
    if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
        signal (SIGTERM,  handler);
}

#ifndef __MINGW32__
/* this code is copied from csh, for printing times */

clock_t time0;
struct tms tm0;

static void
init_time (void)
{
    time0 = times (&tm0);
}


static void
print_time (char *phase)
{
    clock_t time1, wtime;
    double utime, stime;
    struct tms tm1;

#ifndef HZ
    long HZ = sysconf(_SC_CLK_TCK);
#endif

    time1 = times (&tm1);
    utime = (double)(tm1.tms_utime + tm1.tms_cutime -
		     tm0.tms_utime - tm0.tms_cutime) / (double)HZ;
    stime = (double)(tm1.tms_stime + tm1.tms_cstime -
		     tm0.tms_stime - tm0.tms_cstime) / (double)HZ;
    wtime = time1 - time0;

    fprintf (stderr, "%s phase time:  %.2fu %.2fs %lu:%04.1f %.0f%%\n",
		phase, utime, stime, wtime / (60*HZ),
		(double)(wtime % (60*HZ)) / (double)HZ,
		(utime + stime) / ((double)wtime / (double)HZ) * 100.0);
}

#else /* _WIN32 */

#ifndef HZ
 #define HZ 100
#endif
time_t time0;

static void
init_time (void)
{
    time0 = time (NULL);
}

static void
print_time (string phase)
{
    time_t time1, wtime;
    //double utime, stime;
    //struct tms tm1;

    time1 = time(NULL);
    wtime = difftime(time1,time0);

    fprintf (stderr, "%s phase time: %u:%04.1f\n",
                phase, wtime / (60*HZ),
                (double)(wtime % (60*HZ)) / (double)HZ);
}

#endif /* _WIN32 */

/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#if defined(_WIN32)
#include <io.h>
#else
#include <sys/utsname.h>
#endif
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include "phases.h"
#include "lang_defs.h"
#include "string_utils.h"
#include "file_names.h"
#include "file_utils.h"
#include "errors.h"
#include "opt_actions.h"
#include "option_seen.h"
#include "option_names.h"
#include "run.h"
#include "version.h"

extern int errno;

boolean keep_flag = FALSE;

/* linked list of files */
typedef struct file_item_rec {
       char *name;
       int file_descriptor;
       struct file_item_rec *next;
} file_item_t;

string_list_t *count_files = NULL;
static file_item_t *temp_files = NULL;
string_list_t *isystem_dirs = NULL;
static char *tmpdir;
static char *saved_object = NULL;

#if !defined(_WIN32)
#define DEFAULT_TMPDIR  "/tmp"

#else

#include <windows.h>
#define DEFAULT_TMPDIR default_tmpdir()

static char * 
conv_dir_seperator_to_posix (char *bad_path)
{
        int i;
        int bad_path_len = strlen(bad_path);
        for (i = bad_path_len; i-1 >= 0; i--)
          if(bad_path[i] == '\\')
            /* found the location of error, fix it by replacing \\ with / */
            bad_path[i] = '/';
        return bad_path; /* no longer bad, actually */
}

static char * 
conv_dir_seperator_to_win32 (char *bad_path)
{
        int i;
        int bad_path_len = strlen(bad_path);
        for (i = bad_path_len; i-1 >= 0; i--)
          if(bad_path[i] == '/')
            /* found the location of error, fix it by replacing / with \\ */
            bad_path[i] = '\\';
        return bad_path; /* no longer bad, actually */
}

static char * default_tmpdir(void)
{
  static char dir[MAX_PATH];
  if (GetTempPath(MAX_PATH, (LPSTR)dir) == 0)
    return "";
  conv_dir_seperator_to_posix(dir);
  return (char*) dir;
}
#endif /* _WIN32 */

static string_pair_list_t *temp_obj_files = NULL;

/* add file that has already been opened */
static void
add_temp_file_info (char *s, int fd)
{
       file_item_t *p;
       p = (file_item_t *) malloc(sizeof(file_item_t)); 
       p->next = NULL;
       if (temp_files == NULL) {
               temp_files = p;
       } else {
               p->next = temp_files; 
               temp_files = p;
       }
       p->name = s;
       p->file_descriptor = fd;

}

/* add file to list if not already in list */
static void
add_file_if_new (char *s, int fd)
{
       file_item_t *p;
       char *str; 
       for (p = temp_files; p != NULL; p = p->next) {
               if (strcmp(p->name, s) == 0)
                       return;         /* already in list */
       }
       str = string_copy(s);
       /* string not in list */
       add_temp_file_info(str, fd);
}

/* get object file corresponding to src file */
char *
get_object_file (char *src)
{
#if defined(_WIN32) || defined(TARG_NVISA)
	/* user is responsible for specifying writable dir,
	 * don't write it behind their back. */
#else
	// bug 2025
	// Create .o files in /tmp in case the src dir is not writable.
	if (!(keep_flag || (ipa == TRUE) || remember_last_phase == P_any_as)) {
	  char *obj_name = change_suffix(src, "o");
	  string_pair_item_t *p;
	  buffer_t buf;
	  char *mapped_name;
	  FOREACH_STRING_PAIR (p, temp_obj_files) {
	    if (strcmp (STRING_PAIR_KEY(p), obj_name) == 0)
	      return STRING_PAIR_VAL(p);
	  }
	  // Create temp file name as in create_temp_file_name.
	  sprintf(buf, "cco.");
	  mapped_name = tempnam (tmpdir, buf);
	  add_string_pair (temp_obj_files, obj_name, mapped_name);
	  return mapped_name;
	}

	// Handle IPA .o files corresponding to sources with the same basename,
	// e.g., a.c and foo/a.c.  Create unique .o files by substituting '/'
	// in the source name with '%'.  Bugs 9097, 9130.
	if (ipa == TRUE &&
	    !option_was_seen(O_c) &&
	    keep_flag != TRUE) {
	  char *p;
	  src = strcpy(alloca(strlen(src)+1), src);
	  for (p = src; *p != '\0'; p++) {
	    if (*p == '/')
	      *p = '%';
	  }
	}
#endif
	return change_suffix(drop_path(src), "o");
}

/*
 * Need temp file names to be same if use same suffix
 * (because this can be called for both producer and consumer
 * of temp file), but also need names that won't conflict.
 * Put suffix in standard place so have easy way to check 
 * if file already created. 
 * Use tempnam to generate unique file name;
 * tempnam verifies that file is writable.
 */
char *
create_temp_file_name (char *suffix)
{
	buffer_t buf;
	buffer_t pathbuf;
	size_t prefix_len;
	char *s;
	file_item_t *p;
	int fd = -1;
	/* use same prefix as gcc compilers */
	/* use mkstemp instead of tempnam to be more portable */
	sprintf(buf, "cc%s#.XXXXXX", suffix);
	sprintf(pathbuf, "%s/%s", tmpdir, buf); /* full path of tmp files */
#ifdef _WIN32
        /* Canonicalize paths to use forward slashes so that comparisons
           with existing temp_files will work.  */
	conv_dir_seperator_to_posix(pathbuf);
#endif
	/* subtracting the XXXXXX */
	prefix_len = strlen(pathbuf) - strlen(strchr(pathbuf, '#'));

	for (p = temp_files; p != NULL; p = p->next) {
	  if (strncmp(p->name, pathbuf, prefix_len) == 0) {
	    /* matches the prefix and suffix character */
	    return p->name;
	  }
	}
	/* need new file name */
#ifdef _WIN32
	/* mingw doesn't have mkstemp */
	s = mktemp (pathbuf);
	if (!s)
	  internal_error("Couldn't create temporary file %s\n", pathbuf);
	/* Some phases cannot handle backslashes in file names yet.  */
	s = conv_dir_seperator_to_posix(s);
	s = string_copy(s);
#else
	fd = mkstemp(pathbuf);
	s = string_copy(pathbuf);
#endif
	add_temp_file_info(s, fd);
	return s;
}

char *
construct_name (char *src, char *suffix)
{
	if (keep_flag || current_phase == remember_last_phase) {
		char *srcname;
		/* 
		 * if -c -o <name>, then use name.suffix
		 * (this helps when use same source to create different .o's)
		 * if outfile doesn't have .o suffix, don't do this.
		 */
		if (outfile && option_was_seen(O_c) && get_suffix(outfile))
			srcname = outfile;
		else
			srcname = src;
		return change_suffix(drop_path(srcname), suffix);
	} else {
		return create_temp_file_name (suffix);
	}
}

/* use given src name, but check if treated as a temp file or not */
char *
construct_given_name (char *src, char *suffix, boolean keep)
{
	char *s;
	s = change_suffix(drop_path(src), suffix);
	if (keep || current_phase == remember_last_phase) {
		return s;
	} else {
		s = string_copy(s);
		add_file_if_new(s, -1);
		return s;
	}
}

void
mark_saved_object_for_cleanup ( void )
{
	if (saved_object != NULL)
	add_file_if_new(saved_object, -1);
}

/* Create filename with the given extension; eg. foo.anl from foo.f */
char *
construct_file_with_extension (char *src, char *ext)
{
	return change_suffix(drop_path(src),ext);
}

void
init_temp_files (void)
{
#if defined(_WIN32)
        tmpdir = getenv("TMP");
        if (tmpdir == NULL) {
          tmpdir = getenv("TMPDIR");
        }
#else
        tmpdir = string_copy(getenv("TMPDIR"));
#endif
        if (tmpdir == NULL) {
                tmpdir = DEFAULT_TMPDIR;
	} 
#if defined(_WIN32)
        /* avoid space in temp path issues on windows */
	{
	  static char buf[1024];
	  GetShortPathName(tmpdir, buf, 1024);
	  tmpdir = buf;
	}
#endif
	if (!is_directory(tmpdir)) {
		error("$TMPDIR does not exist: %s", tmpdir);
	} 
	else if (!directory_is_writable(tmpdir)) {
		error("$TMPDIR not writable: %s", tmpdir);
	} 
	else if (is_dir_separator(tmpdir[strlen(tmpdir)-1])) {
		/* drop / at end so strcmp matches */
		tmpdir[strlen(tmpdir)-1] = '\0';
	}
	temp_files = NULL;

	temp_obj_files = init_string_pair_list();
}

void
init_count_files (void)
{
        count_files = init_string_list();
}

static char *report_file;

void
init_crash_reporting (void)
{
#if !defined(_WIN32) /* don't generate crash reports on windows */

	#ifdef PSC_TO_OPEN64
	if ((report_file = getenv("OPEN64_CRASH_REPORT")) != NULL)
	#endif
		goto bail;

	#ifdef PSC_TO_OPEN64 
	if (asprintf(&report_file, "%s/open64_crash_XXXXXX", tmpdir) == -1) {
	#endif
		report_file = NULL;
		goto bail;
	}

	if (mkstemp(report_file) == -1) {
		report_file = NULL;
		goto bail;
	}

	#ifdef PSC_TO_OPEN64
	setenv("OPEN64_CRASH_REPORT", report_file, 1);
	#endif
bail:
#endif /* !_WIN32 */
	return;
}

static int save_count;

static int
save_cpp_output (char *path)
{
	char *save_dir, *save_path, *final_path;
	FILE *ifp = NULL, *ofp = NULL;
	char *name = drop_path(path);
	int saved = 0;
#if !defined(_WIN32)
	/* don't generate crash reports on windows;
	 * (for nvisa, how would it be used since not operating on user's 
	 * source?, and is os-specific). */

	struct utsname uts;
	char buf[4096];
	size_t nread;
	char *suffix;
	char *home;
	time_t now;
	int i;

	if (strncmp(name, "cci.", 4) == 0)
		suffix = ".i";
	else if (strncmp(name, "ccii.", 5) == 0)
		suffix = ".ii";
	else
		goto bail;

	if ((ifp = fopen(path, "r")) == NULL)
		goto bail;

	#ifdef PSC_TO_OPEN64
	if ((save_dir = getenv("OPEN64_PROBLEM_REPORT_DIR")) == NULL &&
	    (home = getenv("HOME")) != NULL) {
		asprintf(&save_dir, "%s/.open64-bugs", home);
	}
	#endif

	if (save_dir && mkdir(save_dir, 0700) == -1 && errno != EEXIST) {
		save_dir = NULL;
	}

	if (save_dir == NULL) {
		save_dir = tmpdir;
	}

	asprintf(&save_path, "%s/%s_error_XXXXXX", save_dir, program_name);

	if (mkstemp(save_path) == -1) {
		goto b0rked;
	}

	if ((ofp = fopen(save_path, "w")) == NULL) {
		goto b0rked;
	}
	
	now = time(NULL);
	#ifdef PSC_TO_OPEN64
	fprintf(ofp, "/*\n\nOpen64 compiler problem report - %s",
		ctime(&now));
	fprintf(ofp, "Please report this problem to http://bugs.open64.net/\n");
	#endif
	fprintf(ofp, "If possible, please attach a copy of this file with your "
		"report.\n");
	fprintf(ofp, "\nPLEASE NOTE: This file contains a preprocessed copy of the "
		"source file\n"
		"that may have led to this problem occurring.\n");

	uname(&uts);
	fprintf(ofp, "\nCompiler command line (%s ABI used on %s system):\n",
		abi == ABI_N32 ? "32-bit" : "64-bit",
		uts.machine);

	fprintf(ofp, " ");
	for (i = 0; i < saved_argc; ++i)
		if (saved_argv[i] &&
		    strcmp(saved_argv[i], "-default_options") != 0) {
			int len;
			len = quote_shell_arg(saved_argv[i], buf);
			buf[len] = '\0';
			fprintf(ofp, " %s", buf);
		}
	fprintf(ofp, "\n\n");

	fprintf(ofp, "Version %s build information:\n",
		compiler_version);
	fprintf(ofp, "  Changeset %s\n", cset_id);
	fprintf(ofp, "  Built by %s@%s in %s\n", build_user,
		build_host, build_root);
	fprintf(ofp, "  Build date %s\n", build_date);
	
	if (report_file) {
		int newline = 1;
		struct stat st;
		FILE *rfp;

		if (stat(report_file, &st) == -1)
			goto no_report;
		
		if (st.st_size == 0)
			goto no_report;

		fprintf(ofp, "\nDetailed problem report:\n");
		if ((rfp = fopen(report_file, "r")) == NULL) {
			goto no_report;
		}

		while (fgets(buf, sizeof(buf), rfp) != NULL) {
			int len = strlen(buf);
			if (newline)
				fputs("  ", ofp);
			fputs(buf, ofp);
			newline = buf[len - 1] == '\n';
		}
		if (!newline)
			putc('\n', ofp);

		fclose(rfp);
	}

no_report:	
	if (string_list_size(error_list)) {
		string_item_t *i;
		fprintf(ofp, "\nInformation from compiler driver:\n");
		FOREACH_STRING(i, error_list) {
			fprintf(ofp, "  %s\n", STRING_NAME(i));
		}
	}

	fprintf(ofp, "\nThe remainder of this file contains a preprocessed copy of "
		"the\n"
		"source file that appears to have led to this problem.\n\n*/\n");
	
	while ((nread = fread(buf, 1, sizeof(buf), ifp)) > 0) {
		size_t nwrit;
		if ((nwrit = fwrite(buf, 1, nread, ofp)) < nread) {
			if (nwrit != 0)
				errno = EFBIG;
			goto b0rked;
		}
	}

	#ifdef PSC_TO_OPEN64
	fprintf(ofp, "\n/* End of Open64 compiler problem report. */\n");
	#endif
	
	asprintf(&final_path, "%s%s", save_path, suffix);
	rename(save_path, final_path);

	if (save_count == 0) {
		#ifdef PSC_TO_OPEN64
		fprintf(stderr, "Please report this problem to "
			"http://bugs.open64.net/\n");
		#endif
	}

	fprintf(stderr, "Problem report saved as %s\n", final_path);
	save_count++;
	saved = 1;
	
	goto bail;
#endif /* _WIN32 */

b0rked:
	fprintf(stderr, "Could not save problem report to %s: %s\n",
		save_path, strerror(errno));
bail:
	if (ifp != NULL)
		fclose(ifp);
	if (ofp != NULL)
		fclose(ofp);
		
	return saved;
}

void
cleanup (void)
{
	/* cleanup temp-files */
	file_item_t *p;
	int status;
	if (temp_files == NULL) return;
	for (p = temp_files; p != NULL; p = p->next) {
		if (debug) printf("unlink %s\n", p->name);
                 if (p->file_descriptor > 0){
                       close(p->file_descriptor);
                 }
		/* when using mkstemp, files are always created */
		/* if (execute_flag) { */
			if (internal_error_occurred)
				save_cpp_output(p->name);
#if !defined(_WIN32)
			status = unlink(p->name);
#else
			/* WIN32 unlink does not accept '/' 
			 * as a directory seperator.
			 * So gotta convert that before calling unlink. */
			conv_dir_seperator_to_win32(p->name);
			status = unlink(p->name);
			conv_dir_seperator_to_posix(p->name);
#endif
			if (status != 0 && errno != ENOENT) {
				internal_error("cannot unlink temp file %s", p->name);
				perror(program_name);			
			}
	}
        p = temp_files;
        while (p != NULL) {
                file_item_t *p_next = p->next;
                free(p);
                p = p_next;
        } 
	temp_files = NULL;

	if (save_count) {
		fprintf(stderr, "Please review the above file%s and, "
			"if possible, attach %s to your problem report.\n",
			save_count == 1 ? "" : "s",
			save_count == 1 ? "it" : "them");
	}
}

void
mark_for_cleanup (char *s)
{
	add_file_if_new(s, -1);
}

void
cleanup_temp_objects ()
{
  // Delete the mapped files.
  string_pair_item_t *p;
  FOREACH_STRING_PAIR (p, temp_obj_files) {
    char *s = STRING_PAIR_VAL(p);
    int status = unlink (s);
    if (status != 0 && errno != ENOENT) {
      internal_error("cannot unlink temp object file %s", s);
      perror(program_name);
    }
  }
  if (report_file) {
    unlink(report_file);
  }
}

char *
get_report_file_name()
{
  return report_file;
}

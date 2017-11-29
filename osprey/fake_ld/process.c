/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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

/* ====================================================================
 * ====================================================================
 *
 * Module: process.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/fake_ld/process.c,v $
 *
 * Revision history:
 *  23-Jan-96 - Use fixed name for IPA kept temporary directory.
 *
 * Description:
 *
 * Handles all interprocess operations for ld.
 *
 * ====================================================================
 * ====================================================================
 */


#if defined(__GNUC__)
#include <stdio.h>		/* for sys_errlist */
#endif
#include <stdlib.h>		/* for getenv(3) */
#include <unistd.h>		/* for unlink(2), rmdir(2), etc. */
#include <libgen.h>		/* for basename(3) */
#include <sys/stat.h>		/* for chmod(2) */
#include <sys/mman.h>		/* for mmap(2) */
#include <fcntl.h>		/* for open(2) */
#include <sys/dir.h>		/* for opendir(2), readdir, closedir */
#include <sys/wait.h>		/* for waitpid(2) */
#include <alloca.h>		/* for alloca(3) */
#include <signal.h>		/* for kill(2) */
#include <limits.h>		/* for PATH_MAX */

#ifdef  TARG_MIPS
#include "linker.h"
#include "read.h"
#else
#include "bfd.h"

#define FREE(ptr) \
    free((void *) (ptr))

#define MALLOC_ASSERT(addr) \
    if (addr == 0) msg(ER_FATAL, ERN_MALLOC)

#define MALLOC(nbytes) \
    malloc((size_t)(nbytes))

##define REALLOC(ptr, size) \
    realloc((void *)(ptr), (size_t)(size))

#define UNLINK(path) \
    unlink((const char *)(path))

#define MKDIR(path, mode) \
    mkdir((const char *)(path), (mode_t)(mode))

#define RMDIR(path) \
    rmdir((const char *)(path))	       

#define CLOSE(fid) \
    close((int)(fid))

#define ALLOCA(size) \
    alloca((unsigned int)(size))	       
	       
#define FCHMOD(fid, mode) \
    fchmod((int)(fid), (mode_t)(mode))

#define MUNMAP(addr, len) \
    munmap((void *)(addr), (int)(len))

#define MEMCPY(s1, s2, n) \
    memcpy((void *)(s1), (void *)(s2), (size_t)(n))

define ELF_WORD int

#define OBJ_ASSERT(EX, obj, str) \
    if (!(EX)) {fprintf(stderr,"%s: %s\n", obj->name, str); exit(1);}

#endif


extern char **environ_vars;	    /* list of environment variables */

string toolroot = 0;		    /* set to environment variable TOOLROOT */

static int active_pid;

static mode_t cmask = 0;	    /* file creation mode mask */

static string thisfile = __FILE__;

static string *tmp_list = 0;
static int tmp_list_size = 0;
static int tmp_list_max = 0;
string tmpdir = 0;
static int tmpdir_length;

	/*******************************************************
		Function: dump_argv

		

	 *******************************************************/
static void
dump_argv (string *argv)
{ 
    fputs (argv[0], stderr);
    argv++;
    while (*argv)
	fprintf (stderr, " %s", *argv++);
    fputc ('\n', stderr);

} /* dump_argv */

	/*******************************************************
		Function: do_compile

		Actually perform compilation

	 *******************************************************/
int
do_compile (string *argv)
{
    int pid;
    
    if (toolroot == 0) {
	if ((toolroot = getenv ("TOOLROOT")) == 0)
	    toolroot = "/usr/bin/";
	else
	    toolroot = concat_names (toolroot, "/bin/");
    }

    argv[0] = concat_names (toolroot, basename (argv[0]));

#ifdef  TARG_MIPS
    if (option[OPT_VERBOSE].flag || option[OPT_SHOW].flag)
	dump_argv (argv);
#else
#endif

    pid = pcreateve (argv[0], argv, environ_vars);

    if (pid < 0) {
    	perror(argv[0]);
	exit(1);
    }

    active_pid = pid;
    
    FREE (argv[0]);

    return pid;

} /* do_compile */

	/*******************************************************
		Function: ld_kill_compilation

		

	 *******************************************************/
static void
ld_kill_compilation (int sig)
{
    if (active_pid != 0) {
	kill (active_pid, sig);
	active_pid = 0;
    }

} /* ld_kill_compilation */


	/*******************************************************
		Function: add_to_tmp_file_list

		Maintain list of temp. files created so they are all
		removed on error or when done.  Assume the first entry
		is "tmpdir".

	 *******************************************************/
/*
 * Maintain list of temp. files created so they are all removed on error or
 * when done.  Assume the first entry is "tmpdir".
 */
void
add_to_tmp_file_list (string path)
{
    if (tmp_list_max == 0) {
	tmp_list_max = DEFAULT_TMP_LIST_SIZE;
	tmp_list = (string *) MALLOC (tmp_list_max * sizeof(string));
	MALLOC_ASSERT (tmp_list);
    } else if (tmp_list_size >= tmp_list_max) {
	tmp_list_max *= 2;
	tmp_list = (string *)REALLOC (tmp_list, tmp_list_max * sizeof(string));
	MALLOC_ASSERT (tmp_list);
    }

    tmp_list[tmp_list_size++] = path;

} /* add_to_tmp_file_list */

	/*******************************************************
		Function: remove_from_tmp_file_list

		

	 *******************************************************/
static void
remove_from_tmp_file_list (string path)
{

    if (tmp_list_size == 0)
	return;

    /* if the last entry is not what we need, don't bother searching for it */
    if (tmp_list[tmp_list_size - 1] == path)
	tmp_list_size--;
    
} /* remove_from_tmp_file_list */


	/*******************************************************
		Function: cleanup_all_files

		

	 *******************************************************/
void
cleanup_all_files (void)
{
    int i;

#ifdef  TARG_MIPS
    if (option[OPT_KEEP_TEMPS].flag)
	return;
#else
#endif
    
    for (i = tmp_list_size - 1; i > 0; i--)
	UNLINK (tmp_list[i]);

    if (tmp_list_size >= 1)
	RMDIR (tmp_list[0]);

} /* cleanup_all_files */


	/*******************************************************
		Function: make_temp_file

		Create a unique file

	 *******************************************************/
/* create a unique file */
string
make_temp_file (string name, char suffix)
{
    char path[PATH_MAX];
    int len;
    int count = 1;

    len = strlen (name);
    if (len+4 >= PATH_MAX) {
	fprintf(stderr,%s %s\n","path name too long:", name);
	exit(1);
    }

    strcpy (path, name);

    if (suffix && len >= 2) {
	/* remove the original suffix */
	if (path[len-2] == '.') {
	    len -= 2;
	    path[len] = 0;
	}
    }

    if (suffix) {
	path[len] = '.';
	path[len+1] = suffix;
	path[len+2] = 0;
    }

    if (access (path, F_OK) != 0)
	return copy_of (path);

    do {
	if (suffix)
	    sprintf (&(path[len]), ".%d.%c", count, suffix);
	else
	    sprintf (&(path[len]), "%d", count);
	count++;
    } while (access (path, F_OK) == 0);

    return copy_of (path);

} /* make_temp_file */

/* ====================================================================
 *
 * create_tmpdir
 *
 * Create a temporary directory for (1) relocatable objects generated
 * from the backend under IPA control, (2) IR objects extracted from an
 * archive, and (3) IR objects generated by IPA.
 *
 * There are three cases.  If this is a directory to be kept, because
 * either -keep or a trace flag is specified for an IPA build, then the
 * directory is named <outfilename>.ipakeep, and if it already exists,
 * all files are cleared from it.  If it is a temporary directory for
 * an IPA build, a unique name is used with the template
 * $TMPDIR/<outfilename_with_path_stripped>.ipaXXXXXX .  For a normal link, 
 * a temporary directory
 * is created in the DEFAULT_TMPDIR with the name template XXXXXX.
 *
 * ====================================================================
 */

int
create_tmpdir ( int tracing )
{
    int fixedname = ipa && ( option[OPT_KEEP_TEMPS].flag );

    if ( ipa ) {
	if ( fixedname ) {
	    tmpdir = concat_names ( outfilename, ".ipakeep" );
	} else {
	    char *tmpdir_env_var;
	    if ((tmpdir_env_var = getenv("TMPDIR")) != NULL) {
		char *filename;
	        tmpdir_env_var = concat_names ( tmpdir_env_var, "/");
		if ((filename = strrchr(outfilename, '/')) != NULL)
		    filename++;
		else
		    filename = outfilename;
		
	        tmpdir = concat_names ( tmpdir_env_var, filename);
	    }
	    else
	        tmpdir = outfilename;
	    tmpdir = concat_names ( tmpdir, ".ipaXXXXXX" );
	}
    } else {
	tmpdir = concat_names ( DEFAULT_TMPDIR, "XXXXXX" );
    }
    if ( ! fixedname ) {
	tmpdir = mktemp ( tmpdir );
    }
    tmpdir_length = strlen ( tmpdir );

    if ( cmask == 0 ) {
	cmask = umask (0);
	(void) umask (cmask);
    }

    if ( MKDIR (tmpdir, 0777 & ~cmask) != 0 ) {
	if ( errno == EEXIST && fixedname ) {
	    /* We have an old instance of this directory -- clear it out: */
	    DIR *dirp;
	    struct direct *entryp;
	    char *prefix;

	    dirp = opendir ( tmpdir );
	    if ( dirp != NULL ) {
		prefix = concat_names ( tmpdir, "/" );
		while ( ( entryp = readdir(dirp) ) != NULL ) {
		    /* Don't bother with names of one or two characters, e.g. '.'
		     * and '..', since we don't create temporary files with such
		     * names:
		     */
#if defined(_DIRENT_HAVE_D_NAMLEN)
		  if ( entryp->d_namlen > 2 )
#else
		  if (_D_EXACT_NAMLEN(entryp) > 2)
#endif
		    {
			string fname = concat_names ( prefix, entryp->d_name);
			unlink (fname);
			FREE (fname);
		    }
		}
		FREE (prefix);
		closedir ( dirp );
	    }
	} else {
    	    perror(""cannot create temporary directory for code generation");
	    return -1;
	}
    }

    add_to_tmp_file_list ( tmpdir );

    return 0;

} /* create_tmpdir */

	/*******************************************************
		Function: create_unique_file

		

	 *******************************************************/
string
create_unique_file (string path, char suffix)
{
    string p;
    string base = basename (path);
    string new_path;
    int fd;

    /* length of tmpdir + basename of path and '/' between the dir
       and the basename + null terminator */
    p = (string) MALLOC (tmpdir_length + strlen(base) + 2);
    MALLOC_ASSERT (p);
    strcpy (p, tmpdir);
    strcat (p, "/");
    strcat (p, base);
    new_path = make_temp_file (p, suffix);
    FREE (p);

    if ((fd = creat (new_path, 0666 & ~cmask)) == -1) {
	perror(new_path);
	exit(1)
    }

    CLOSE (fd);

    return new_path;

} /* create_unique_file */


string
create_tmp_file (string filename)
{
    static string tmppath = "/tmp/";
    static string tmpsuffix = "XXXXXX";
    string tmp_file_name;
    string ret_file_name;
    FILE *file;

    tmp_file_name = (string)MALLOC(strlen(tmppath) + strlen(filename) + strlen(tmpsuffix) + 1);
    strcpy(tmp_file_name, tmppath);
    strcat(tmp_file_name, filename);
    strcat(tmp_file_name, tmpsuffix);

    tmp_file_name = mktemp(tmp_file_name);
    if ((file = fopen(tmp_file_name, "w+")) == NULL) {
      perror(tmp_file_name);
      exit(1);
    }
    fclose(file);

    ret_file_name = ipa_copy_of(tmp_file_name);
    FREE (tmp_file_name);

    return ret_file_name;
}

	/*******************************************************
		Function: get_command_line

		

	 *******************************************************/
string *
get_command_line(bfd *abfd, 
    	    	 string in_path, 
		 string out_path, 
		 int *arg_count)
{
    static string default_compilation_flags[] = DEFAULT_COMPILATION_FLAGS;
    int i;
    int argc = 0;
    string *old_argv, *new_argv;
    Elf_Internal_Ehdr *ehdr = elf_elfheader (abfd);

    for (i = 1; i < ehdr->e_shnum; i++)
    	Elf_Internal_Shdr *shdr = elf_elfsections (abfd)[i];

	if (shdr->sh_info == WT_COMP_FLAGS) {
	    char *base_addr;
	    int j;
	    ELF_WORD *args;

	    if (shdr->sh_size <= 1)
		continue;

	    read_one_section (i, abfd);
	    base_addr = (char *) pobj->p_xelf_shdr[i].p_raw;
	    argc = (int)(*((ELF_WORD *) base_addr));

	    args = (ELF_WORD *) (base_addr + sizeof(ELF_WORD));
	    old_argv = (string *) ALLOCA (sizeof(string) * argc);
	    MALLOC_ASSERT (old_argv);
	    
	    for (j = 0; j < argc; j++) {
		OBJ_ASSERT (args[j] < shdr->sh_size, pobj,
			    "invalid WT_COMP_FLAGS WHIRL section");
		old_argv[j] = base_addr + args[j];
	    }

	    break;
	}

    if (argc == 0) {
	argc = DEFAULT_COMPILATION_ARGC;
	old_argv = default_compilation_flags;
    }

    new_argv = (string *) MALLOC ((argc + 6) * sizeof(string));
    MALLOC_ASSERT (new_argv);

    for (i = 0; i < argc; i++)
	new_argv[i] = old_argv[i];

#ifdef  TARG_MIPS
    switch (targos) {
    case TOS_MIPS_O32:
	new_argv[argc++] = "-o32";
	break;
    case TOS_MIPS_N32:
	new_argv[argc++] = "-n32";
	break;
    case TOS_MIPS_64:
	new_argv[argc++] = "-64";
	break;
    case TOS_IA64_64:
	new_argv[argc++] = "-i64";
	break;
    case TOS_IA64_32:
	new_argv[argc++] = "-i32";
	break;
    }
#else
    new_argv[argc++] = "-64";
#endif

    new_argv[argc++] = in_path;
    new_argv[argc++] = "-o";
    new_argv[argc++] = out_path;
    new_argv[argc++] = "-c";
    new_argv[argc] = 0;
    
    *arg_count = argc;

    return new_argv;

} /* get_command_line */


	/*******************************************************
		Function: extract_archive_member

		Given an archive of WHIRL objects, extract the one
		specified and put it into a separate file.

	 *******************************************************/
static int
extract_archive_member (bfd *abfd, string path)
{
    int fd = -1;
    int mode = 0666;
    pointer addr = (pointer)-1;
    struct ar_hdr *p_hdr = (ar_hdr *)abfd->arelt_data->arch_header;

    if ((fd = OPEN (path, O_RDWR|O_CREAT|O_TRUNC, mode)) != -1)
	addr = (pointer) MMAP ( 0, 
	    	    	    	p_hdr->ar_size,  
				PROT_READ|PROT_WRITE,
    	    	    	    	MAP_SHARED, 
				fd, 
				0);
	
    if (fd == -1 || addr == (pointer)-1 || FCHMOD (fd, mode) != 0 ) {
    	perror("cannot create intermediate file");
    	return -1;
    }

    CLOSE (fd);

    MEMCPY (addr, bfd_tell(abfd), p_hdr->ar_size);

    MUNMAP (addr, p_hdr->ar_size);

    return 0;

} /* extract_archive_member */

	/*******************************************************
		Function: make_link

		

	 *******************************************************/
int
make_link (const string dest, const string src)
{
    static string working_dir = 0;
    int link_result;

    LD_ASSERT (dest && src, thisfile,
	       "NULL path name passed to symbolic_link()");

    /* try hard link first */

    if (link (dest, src) == 0)
	return 0;
    
    /* hard link fails, try symbolic link */

    if (dest[0] == '/')
	link_result = symlink (dest, src);
    else {
	string new_dest;
	
	if (working_dir == 0) {
	    string tmp;
	    working_dir = getcwd ((char *) NULL, PATH_MAX);
	    if (working_dir == NULL) {
		perror("getcwd(3)");
		exit(1);
    	    }
	    tmp = working_dir;
	    working_dir = concat_names (working_dir, "/");
	    FREE (tmp);
	}

	new_dest = concat_names (working_dir, dest);
	link_result = symlink (new_dest, src);
	FREE (new_dest);
    }

    return link_result;
    
} /* make_link */

	/*******************************************************
		Function: ld_compile

		

	 *******************************************************/
string
ld_compile (bfd *abfd)
{
    string input_path;
    string output_path;
    int argc;
    string *argv;
    int child_pid;
    int statptr;
    
    if (tmpdir == 0)
	if (create_tmpdir (FALSE) != 0)
	    msg(ER_FATAL, ERN_COMPILE, abfd->filename);

    if ((input_path = create_unique_file (abfd->filename, 'B')) == 0)
	msg(ER_FATAL, ERN_COMPILE, abfd->filename);
    
    if (pobj->ftype == FT_AR) {
	if (extract_archive_member (abfd, input_path) != 0)
	    msg(ER_FATAL, ERN_COMPILE, abfd->filename);
    } else {
	UNLINK (input_path);
	if (make_link (abfd->filename, input_path) != 0)
	    msg(ER_FATAL, ERN_COMPILE, abfd->filename);
    }

    if ((output_path = create_unique_file (abfd->filename, 'o')) == 0)
	msg(ER_FATAL, ERN_COMPILE, abfd->filename);

    add_to_tmp_file_list (output_path);
    add_to_tmp_file_list (input_path);


    argv = get_command_line (abfd, input_path, output_path, &argc);
    
    msg(ER_DEFAULT /* ER_VERBOSE */ , ERN_MESSAGE2, "Compiling", abfd->filename);
    child_pid = do_compile (argv);

    (void) waitpid (child_pid, &statptr, 0);
    if (statptr != 0 && WEXITSTATUS(statptr) != 0)
	msg(ER_FATAL, ERN_COMPILE, pobj->name);
    
    active_pid = 0;
    
    FREE (argv);

    UNLINK (input_path);
    remove_from_tmp_file_list (input_path);
    FREE (input_path);

    return output_path; 

} /* ld_compile */

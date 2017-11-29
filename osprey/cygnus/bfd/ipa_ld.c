/*

  Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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

  Contact information:  PathScale, Inc., 2071 Stierlin Court, Suite 200,
  Mountain View CA 94043, USA, or:

  http://www.pathscale.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

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
#include <errno.h>
#include <string.h>
#include <dlfcn.h>

#include "aout/ar.h"

#include "bfd.h"
#include "libbfd.h"
#include "elf-bfd.h"

#include "ipa_ld.h"

#define DEFAULT_TOOLROOT "/usr/bin/sgicc"

extern bfd_boolean is_ipa;

extern struct bfd_link_info link_info; /* defined in ld/ldmain.c */
extern char **environ_vars;	    /* list of environment variables */

#if !defined(EF_IRIX_ABI64)
#define EF_IRIX_ABI64   0x00000010
#endif

extern void process_whirl64(void *, off_t, void *, int, const char *) __attribute__((weak));
extern void process_whirl32(void *, off_t, void *, int, const char *) __attribute__((weak));
extern void *ipa_open_input(char *, off_t *) __attribute__((weak));

void *(*p_ipa_open_input)(char *, off_t *) = NULL;
void (*p_ipa_init_link_line)(int, char **) = NULL;
void (*p_ipa_add_link_flag)(const char*) = NULL;
void (*p_ipa_modify_link_flag)(char*, char*) = NULL;
void (*p_ipa_driver)(int, char **) = NULL;
#ifdef OSP_OPT
void (*p_process_whirl64)(void *, off_t, void *, int, const char *, off_t, bfd_boolean) = NULL;
void (*p_process_whirl32)(void *, off_t, void *, int, const char *, off_t, bfd_boolean) = NULL;
#else
void (*p_process_whirl64)(void *, off_t, void *, int, const char *, off_t) = NULL;
void (*p_process_whirl32)(void *, off_t, void *, int, const char *, off_t) = NULL;
#endif
int  (*p_Count_elf_external_gots)(void) = NULL;
void (*p_ipa_insert_whirl_marker)(void) = NULL;
void (*p_Sync_symbol_attributes)(unsigned int, unsigned int, bfd_boolean, unsigned int) = NULL;
#ifdef KEY
void (*p_ipa_erase_link_flag)(const char*) = NULL;
void (*p_Ipalink_Set_Error_Phase)(char *) = NULL;
void (*p_Ipalink_ErrMsg_EC_infile)(char *) = NULL;
void (*p_Ipalink_ErrMsg_EC_outfile)(char *) = NULL;
#endif

string toolroot = 0;		    /* set to environment variable TOOLROOT */

static int active_pid;

static mode_t cmask = 0;	    /* file creation mode mask */

static string thisfile = __FILE__;

static char *default_path = "/usr/ia64-sgi-linux/lib/gcc-lib/ia64-sgi-linux/sgicc-1.0";
static char *env_name = "LD_LIBRARY_PATH";

static string *tmp_list = 0;
static int tmp_list_size = 0;
static int tmp_list_max = 0;
string tmpdir = 0;
static int tmpdir_length = 0;
static bfd *p_current_bfd = NULL;

string outfilename = "./a.out";
string WB_flags = NULL;
string Y_flags = NULL;

LD_IPA_OPTION ld_ipa_opt[] = {
/************************************************/
/*  ld_ipa_option_enum	    flag	    set */
/************************************************/
    {LD_IPA_SHARABLE, 	    F_CALL_SHARED,  F_CALL_SHARED}, 
    {LD_IPA_DEMANGLE, 	    0,		    0}, 
    {LD_IPA_SHOW, 	    0,		    0}, 
    {LD_IPA_HIDES, 	    0,		    0}, 
    {LD_IPA_TARGOS, 	    TOS_IA64_64,    0}, 
    {LD_IPA_ISA,	    0,		    0},
    {LD_IPA_XXXX, 	    0,		    0}, 
    {LD_IPA_XXXX, 	    0,		    0}
};



	/*******************************************************
		Dummy functions and variables so we don't
		have to muck with ipa sources.

		
	 *******************************************************/

char * always_demangle(char *name, char Demangle){return NULL;}
char *__Release_ID;
void read_one_section(int index, void *p_void){return;}

void merge_ext(void *p_sym, char *name, int num, void *p_obj) 
    {return;}

void msg (int type, int msg_id, ...) {return;}


	/*******************************************************
		Function: ipa_copy_of

		Allocate for and copy given string into a copy.

	 *******************************************************/
char *
ipa_copy_of (char *str)
{
    register int len;
    register char *p;

    len = strlen(str) + 1;
    p = (char *) MALLOC (len);
    MALLOC_ASSERT (p);
    BCOPY (str, p, len);
    return p;
} /* ipa_copy_of */


	/*******************************************************
		Function: concat_names

		Create a new string by concating 2 other strings.

	 *******************************************************/
string
concat_names(const string name1, const string name2)
{
    char *mangled_name = NULL;
    int len = strlen(name1)+strlen(name2)+1;

    mangled_name = (char *)MALLOC(len);
    MALLOC_ASSERT(mangled_name);

    strcpy(mangled_name, name1);
    strcat(mangled_name, name2);

    return(mangled_name);
}

	/*******************************************************
		Function: dump_argv

		This routines depends on the last argv element
		being NULL and no others.

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
    
    if (toolroot) {
	if ((toolroot = getenv ("TOOLROOT")) == 0)
	    toolroot = ipa_copy_of(DEFAULT_TOOLROOT);
	else
	    toolroot = concat_names (toolroot, DEFAULT_TOOLROOT);
    }

    argv[0] = concat_names (toolroot, basename (argv[0]));

    if (ld_ipa_opt[LD_IPA_VERBOSE].flag || ld_ipa_opt[LD_IPA_SHOW].flag)
	dump_argv (argv);

    pid = fork();
    pid = execve(argv[0], argv, environ_vars);

    if (pid < 0) {
    	perror(argv[0]);
	exit(1);
    }

    active_pid = pid;
    
    FREE (argv[0]);
    argv[0] = NULL;

    if (toolroot) {
    	FREE (toolroot);
	toolroot = NULL;
    }

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

    if (ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag)
	return;
    
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
	fprintf(stderr,"%s %s\n","path name too long:", name);
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
	return ipa_copy_of (path);

    do {
	if (suffix)
	    sprintf (&(path[len]), ".%d.%c", count, suffix);
	else
	    sprintf (&(path[len]), "%d", count);
	count++;
    } while (access (path, F_OK) == 0);

    return ipa_copy_of (path);

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
    int fixedname = is_ipa && ( ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag );

    if ( is_ipa ) {
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
    	    perror("cannot create temporary directory for code generation");
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
create_unique_file (const string path, char suffix)
{
    string p;
    string base = basename (path);
    string new_path;
    int fd;

    /* length of tmpdir + basename of path and '/' between the dir
       and the basename + null terminator */
    p = (string) MALLOC (strlen(tmpdir) + strlen(base) + 2);
    MALLOC_ASSERT (p);
    strcpy (p, tmpdir);
    strcat (p, "/");
    strcat (p, base);
    new_path = make_temp_file (p, suffix);
    FREE (p);

    if ((fd = creat (new_path, 0666 & ~cmask)) == -1) {
	perror(new_path);
	exit(1);
    }

    CLOSE (fd);
    
    return new_path;

} /* create_unique_file */


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
    string *old_argv;
    string *new_argv;
    Elf_Internal_Ehdr *ehdr = elf_elfheader (abfd);
    asection *p_asec;

    for (i = 1; i < ehdr->e_shnum; i++) {
    	Elf_Internal_Shdr *p_shdr = elf_elfsections (abfd)[i];

	if (p_shdr->sh_info == WT_COMP_FLAGS) {
	    char *base_addr;
	    int j;
	    ELF_WORD *args;

	    if (p_shdr->sh_size <= 1)
		continue;

	    base_addr = (char *) p_shdr->contents;
	    argc = (int)(*((ELF_WORD *) base_addr));

	    args = (ELF_WORD *) (base_addr + sizeof(ELF_WORD));
	    old_argv = (string *) ALLOCA (sizeof(string) * argc);
	    MALLOC_ASSERT (old_argv);
	    
	    for (j = 0; j < argc; j++) {
		OBJ_ASSERT (args[j] < p_shdr->sh_size, abfd,
			    "invalid WT_COMP_FLAGS WHIRL section");
		old_argv[j] = base_addr + args[j];
	    }

	    break;
	}
    }

    if (argc == 0) {
	argc = DEFAULT_COMPILATION_ARGC;
	old_argv = default_compilation_flags;
    }

    new_argv = (string *) MALLOC ((argc + 6) * sizeof(string));
    MALLOC_ASSERT (new_argv);

    for (i = 0; i < argc; i++)
	new_argv[i] = old_argv[i];

    new_argv[argc++] = "-64";

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
    struct areltdata *p_areltdata = (struct areltdata *)abfd->arelt_data;
    struct ar_hdr *p_hdr = arch_hdr(abfd);

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
    string file_name = (string)abfd->filename;
    
    if (tmpdir == 0)
	if (create_tmpdir (FALSE) != 0) {
	    fprintf(stderr,"create_tmpdir() failed for %s\n",abfd->filename);
    	    exit(1);
	}

    if ((input_path = create_unique_file (file_name, 'B')) == 0) {
	    fprintf(stderr,"create_unique_file() failed for %s\n",abfd->filename);
    	    exit(1);
    }
    
    if (abfd->arelt_data) {
	if (extract_archive_member (abfd, input_path) != 0) {
	    fprintf(stderr,"extract_archive_member() failed for %s\n",abfd->filename);
    	    exit(1);
	}
    } else {
	UNLINK (input_path);
	if (make_link (file_name, input_path) != 0) {
	    fprintf(stderr,"make_link() failed for %s\n",abfd->filename);
    	    exit(1);
	}
    }

    if ((output_path = create_unique_file (file_name, 'o')) == 0)
	if (create_tmpdir (FALSE) != 0) {
	    fprintf(stderr,"create_unique_file() failed for %s\n",abfd->filename);
    	    exit(1);
    	}

    add_to_tmp_file_list (output_path);
    add_to_tmp_file_list (input_path);


    argv = get_command_line (abfd, input_path, output_path, &argc);
    

    if (ld_ipa_opt[LD_IPA_VERBOSE].flag || ld_ipa_opt[LD_IPA_SHOW].flag)
    	fprintf(stderr,"Compiling %s\n",abfd->filename);

    child_pid = do_compile (argv);

    (void) waitpid (child_pid, &statptr, 0);
    if (statptr != 0 && WEXITSTATUS(statptr) != 0)
    	{
	fprintf(stderr,"Compile of %s failed!\n",abfd->filename);
	exit(1);
	}

    active_pid = 0;
    
    FREE (argv);

    UNLINK (input_path);
    remove_from_tmp_file_list (input_path);
    FREE (input_path);

    return output_path; 

} /* ld_compile */

	/*******************************************************
		Function: ld_slookup_mext

		Return pointer to ?? struct.
	 *******************************************************/
void *
ld_slookup_mext(char *name, bfd_boolean is_extern)
{
    bfd *abfd = p_current_bfd;
    struct elf_link_hash_entry *p_hash = NULL;

    if (!is_extern)
    	p_hash = elf_link_hash_lookup (  elf_hash_table (&link_info), 
    	    	    	    	    name, 
				    bfd_false, 
				    bfd_false, 
				    bfd_false);
    else
    	p_hash = ((struct elf_link_hash_entry *)
    	    bfd_wrapped_link_hash_lookup(   abfd, 
    	    	    	    	    	    &link_info, 
					    name, 
					    bfd_false, 
					    bfd_false, 
					    bfd_false));

    return p_hash;


}

	/*******************************************************
		Function: ld_set_st_idx

		This field cannot be used beyond
		the pass1 phase.
	 *******************************************************/
void
ld_set_st_idx (void *pext, int st_idx)
{
    struct elf_link_hash_entry *p_hash = (struct elf_link_hash_entry *)pext;

    p_hash->ipa_indx = st_idx;
}

	/*******************************************************
		Function: ld_get_st_idx

		This field cannot be used beyond
		the pass1 phase.
	 *******************************************************/
int
ld_get_st_idx (void *pext)
{
    struct elf_link_hash_entry *p_hash = (struct elf_link_hash_entry *)pext;

    return p_hash->ipa_indx;
}


	/*******************************************************
		Function: ipa_set_ndx

		This field cannot be used beyond
		the pass1 phase.
	 *******************************************************/
int
ipa_set_ndx (bfd *abfd)
{
    if (ipa_is_whirl(abfd))
      	return WHIRL_ST_IDX_UNINITIALIZED;
    else
      	return WHIRL_ST_IDX_NOT_AVAILABLE;
}

	/*******************************************************
		Function: ipa_set_def_bfd


	 *******************************************************/
void
ipa_set_def_bfd(bfd *abfd, struct bfd_link_hash_entry *p_bfd_hash)
{
    if (is_ipa)
    	p_bfd_hash->u.def.section = (asection *)abfd;
}

	/*******************************************************
		Function: ld_resolved_to_obj

		
	 *******************************************************/
bfd_boolean
ld_resolved_to_obj (void *pext, void *pobj)
{
    struct elf_link_hash_entry *p_hash;
    bfd *abfd = NULL;

    if (pext)
    	p_hash = (struct elf_link_hash_entry *)pext;
    else
    	p_hash = NULL;
	
    switch(p_hash->root.type) {

    	case bfd_link_hash_common:
	    abfd = p_hash->root.u.c.p->section->owner;
	    break;

	case bfd_link_hash_undefined:
	case bfd_link_hash_undefweak:
	    abfd = p_hash->root.u.undef.abfd;
	    break;

	case bfd_link_hash_defined:
	case bfd_link_hash_defweak:
	    abfd = (bfd *)p_hash->root.u.def.section;
	    break;

	case bfd_link_hash_new:
	case bfd_link_hash_indirect:
	case bfd_link_hash_warning:
	default:
	    break;
	    
    }

    return (abfd == (struct _bfd *)pobj);
}


	/*******************************************************
		Function: ld_get_section_base

		Return the raw address of the given section
		the pass1 phase.
	 *******************************************************/
char *
ld_get_section_base (void *pobj, int sect_ndx)
{
    const bfd *abfd = (bfd *) pobj;
    Elf_Internal_Shdr **i_shdrp = elf_elfsections (abfd);
    Elf_Internal_Shdr *p_shdr = i_shdrp[sect_ndx];

    return (char *)abfd->usrdata+p_shdr->sh_offset;
}

	/*******************************************************
		Function: ld_get_section_size

		
	 *******************************************************/
unsigned long long
ld_get_section_size(void *pobj, int sect_ndx)
{
    const bfd *abfd = (bfd *) pobj;
    Elf_Internal_Shdr **i_shdrp = elf_elfsections (abfd);

    return ((unsigned long long)i_shdrp[sect_ndx]->sh_size);
}

	/*******************************************************
		Function: ld_get_section_name

		
	 *******************************************************/
char *
ld_get_section_name(void *pobj, int sect_ndx)
{
    bfd *abfd = (bfd *) pobj;
    Elf_Internal_Shdr **i_shdrp = elf_elfsections (abfd);
    Elf_Internal_Shdr *p_shdr = i_shdrp[sect_ndx];
    char *tbl = bfd_elf_get_str_section(abfd, 
    	    	    	    	    	elf_elfheader (abfd)->e_shstrndx);

    return &tbl[p_shdr->sh_name];
}

	/*******************************************************
		Function: ld_get_mmap_addr

		
	 *******************************************************/
void *
ld_get_mmap_addr(void *pobj)
{
    const bfd *abfd = (bfd *) pobj;

    return (void *)abfd->usrdata;
    
}

	/*******************************************************
		Function: ld_set_section_data

		
	 *******************************************************/
void 
ld_set_section_data(void *pobj,int ndx)
{
    bfd *abfd = (bfd *) pobj;
    Elf_Internal_Shdr **i_shdrp = elf_elfsections (abfd);
    Elf_Internal_Shdr *p_shdr = i_shdrp[ndx];
    size_t size = p_shdr->sh_size;
    char *buf;
    
    if (!size)
    	return;
	
    buf = ((char *) bfd_malloc (size));
    if (buf == NULL) {
    	fprintf(stderr,"bfd_malloc failed in ld_set_section_data for %s\n",abfd->filename);
    	exit(1);
    }

    if (!p_shdr->contents) {
    	if (bfd_seek (	abfd, p_shdr->sh_offset, SEEK_SET) != 0) {
    	    fprintf(stderr,"bfd_seek failed in ld_set_section_data for %s\n",abfd->filename);
    	    exit(1);
	}
	if (bfd_read (	(PTR) buf, 1, size, abfd) != size) {
    	    fprintf(stderr,"Bfd_read failed in ld_set_section_data for %s\n",abfd->filename);
    	    exit(1);
	}
     }
    
    return;

}

	/*******************************************************
		Function: ld_release_section_data

		
	 *******************************************************/
void
ld_release_section_data(void *pobj,int ndx)
{
    const bfd *abfd = (bfd *) pobj;
    Elf_Internal_Shdr **i_shdrp = elf_elfsections (abfd);
    Elf_Internal_Shdr *p_shdr = i_shdrp[ndx];

    if (p_shdr->contents) {
    	free(p_shdr->contents);
	p_shdr->contents = NULL;
 }
}

	/*******************************************************
		Function: Count_elf_external_gots

		
	 *******************************************************/
int 
Count_elf_external_gots (void)
{
    return(20);     /* This is until we figure out how to estimate for ia64 */
}

	/*******************************************************
		Function: ld_set_cur_bfd


	 *******************************************************/
void 
ld_set_cur_obj(bfd *abfd)
{
    p_current_bfd = (bfd *)abfd;
}


	/*******************************************************
		Function: ld_get_cur_bfd


	 *******************************************************/
void *
ld_get_cur_obj(void)
{
    return p_current_bfd;
}

	/*******************************************************
		Function: ipa_is_whirl


	 *******************************************************/
#define ET_SGI_IR   (ET_LOPROC + 0)

bfd_boolean
ipa_is_whirl(bfd *abfd)
{
    Elf_Internal_Ehdr *i_ehdrp;	/* Elf file header, internal form */

    i_ehdrp = elf_elfheader (abfd);

    if (i_ehdrp->e_type == ET_SGI_IR) {
    	    return(TRUE);
    }

    return(FALSE);
}


	/*******************************************************
		Function: ipa_process_whirl

		I need to read the WHIRL symbol table so the
		internal mechanisms of IPA will have their 
		data structures correctly filled out.
		
		Since IPA needs an mmapped view of the object
		I'm trying to remap it here. It is not ready
		for archives yet.
		
		I am overloading the usrdata field of the bfd
		with the assumption that bfd is done with it.
		
	 *******************************************************/
void
ipa_process_whirl ( bfd *abfd) 
{

    off_t mapped_size;
    abfd->usrdata = (PTR)(*p_ipa_open_input)((char *)abfd->filename, &mapped_size);

#if !defined(__ALWAYS_USE_64BIT_ELF__)
    /* Should be sync. with Config_Target_From_ELF() defined in be.so
     */
    if( ( elf_elfheader (abfd)->e_flags & EF_IRIX_ABI64 ) == 0 )
      (*p_process_whirl32) ( 
			    (void *)abfd, 
			    elf_elfheader (abfd)->e_shnum, 
			    abfd->usrdata+elf_elfheader(abfd)->e_shoff,
			    0, /* check_whirl_revision */
#ifdef OSP_OPT
			    abfd->filename, mapped_size, FALSE);
#else
                            abfd->filename, mapped_size);
#endif
    else
#endif    
      (*p_process_whirl64) ( 
			    (void *)abfd, 
			    elf_elfheader (abfd)->e_shnum, 
			    abfd->usrdata+elf_elfheader(abfd)->e_shoff,
			    0, /* check_whirl_revision */
#ifdef OSP_OPT
			    abfd->filename, mapped_size, FALSE);
#else
                            abfd->filename, mapped_size);
#endif
}

#ifdef OSP_OPT
    /*******************************************************
        Function: ipa_mmap_file_in_archive

        If the whirl is in an archive, this function
        tries to mmap the file to memory, and return
        the mmaped address.

     *******************************************************/

char *ipa_mmap_file_in_archive ( bfd *bfd, int fd, off_t mapped_size) {
  off_t offset = bfd->origin % getpagesize();
  char *buf = (char *)mmap(0, mapped_size + offset, PROT_READ|PROT_WRITE,
                     MAP_PRIVATE, fd, bfd->origin - offset);

  if (buf == (char *)(-1)) {
    return NULL;
  }

  return buf + offset;
}

#endif

    /*******************************************************
        Function: ipa_process_whirl_in_archive

        If the whirl is in a mixed archive, this
        archive should already be opened. So this
        function just map the mmap pointer to the
        correct position in the archive.

     *******************************************************/

void ipa_process_whirl_in_archive ( bfd *abfd, bfd *element)
{
  char *buf;
  struct ar_hdr *p_hdr;
  off_t mapped_size;
  ld_set_cur_obj(element);
  p_hdr = arch_hdr(element);
  mapped_size = strtol (p_hdr->ar_size, NULL, 10);

#ifdef OSP_OPT
  int fd = open(abfd->filename, O_RDONLY);
  if ((buf = ipa_mmap_file_in_archive(element, fd, mapped_size)) == NULL) {
    einfo(("%F%B: ipa_mmap_file_in_archive failed for member %B\n"), abfd, element);
  }
#else
  if ((buf = bfd_alloc(element, mapped_size)) == NULL) {
    einfo(("%F%B: bfd_alloc failed for member %B\n"), abfd, element);
  }
  if (bfd_seek(element, 0, SEEK_SET) != 0) {
    einfo(("%F%B: bfd_seek failed for member %B\n"), abfd, element);
  }
  if (bfd_bread(buf, mapped_size, element) != mapped_size) {
    einfo(("%F%B: bfd_read failed for member %B\n"), abfd, element);
  }
#endif
  element->usrdata = buf;

#if !defined(__ALWAYS_USE_64BIT_ELF__)
  /* Should be sync. with Config_Target_From_ELF() defined in be.so
   */
  if( ( elf_elfheader (element)->e_flags & EF_IRIX_ABI64 ) == 0 )
    (*p_process_whirl32) (
			  (void *)element,
			  elf_elfheader (element)->e_shnum,
			  element->usrdata+elf_elfheader(element)->e_shoff,
			  0, /* check_whirl_revision */
#ifdef OSP_OPT
			  abfd->filename, mapped_size, TRUE);
#else
                          abfd->filename, mapped_size);
#endif
  else
#endif
    (*p_process_whirl64) (
			  (void *)element,
			  elf_elfheader (element)->e_shnum,
			  element->usrdata+elf_elfheader(element)->e_shoff,
			  0, /* check_whirl_revision */
#ifdef OSP_OPT
			  element->filename, mapped_size, TRUE);
#else
                          element->filename, mapped_size);
#endif
}

	/*******************************************************
		Function: ipa_set_syms

		dlopen ipa.so and set entry points with
		dlsym calls.

	 *******************************************************/
void
ipa_set_syms(void)
{

    void *p_handle = NULL;
    char *p_error = NULL;

    p_handle = dlopen("ipa.so",RTLD_LAZY);
    if (!p_handle) {
    	fputs (dlerror(), stderr);
    	exit(1);
    }
    
    p_ipa_open_input = dlsym(p_handle,"ipa_open_input");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_ipa_init_link_line = dlsym(p_handle,"ipa_init_link_line");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_ipa_add_link_flag = dlsym(p_handle,"ipa_add_link_flag");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_ipa_modify_link_flag = dlsym(p_handle,"ipa_modify_link_flag");
    if ((p_error = dlerror()) != NULL) {
      fputs(p_error, stderr);
      exit(1);
    }
    p_ipa_driver = dlsym(p_handle,"ipa_driver");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_process_whirl64 = dlsym(p_handle,"process_whirl64");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_process_whirl32 = dlsym(p_handle,"process_whirl32");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_ipa_insert_whirl_marker = dlsym(p_handle,"ipa_insert_whirl_marker");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_Sync_symbol_attributes = dlsym(p_handle,"Sync_symbol_attributes");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

#ifdef KEY
    p_ipa_erase_link_flag = dlsym(p_handle,"ipa_erase_link_flag");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_Ipalink_Set_Error_Phase = dlsym(p_handle,"Ipalink_Set_Error_Phase");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_Ipalink_ErrMsg_EC_infile = dlsym(p_handle,"Ipalink_ErrMsg_EC_infile");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }

    p_Ipalink_ErrMsg_EC_outfile = dlsym(p_handle,"Ipalink_ErrMsg_EC_outfile");
    if ((p_error = dlerror()) != NULL)  {
    	fputs(p_error, stderr);
    	exit(1);
    }
#endif
}

	/*******************************************************
		Function: ipa_symbol_sync

		

	 *******************************************************/
static bfd_boolean
ipa_symbol_sync(struct bfd_link_hash_entry *p_bfd_link_hash, PTR info)
{
    const char *name;
    bfd_boolean is_undef = FALSE;
    struct elf_link_hash_entry *p_elf_link_hash ;
    unsigned int result = 0;
    bfd_boolean is_weak = FALSE;

    name = p_bfd_link_hash->root.string;

    if (!name)
    	return(TRUE);


    switch (p_bfd_link_hash->type) {
    	case bfd_link_hash_undefined:
	    is_undef = TRUE;
	    break;
	case bfd_link_hash_undefweak:
	    is_undef = TRUE;
	    is_weak = TRUE;
	    break;
	case bfd_link_hash_defined:
	    is_undef = FALSE;
	    break;
	case bfd_link_hash_defweak:
	    is_undef = FALSE;
	    is_weak = TRUE;
	    break;
	case bfd_link_hash_common:
	    is_undef = FALSE;
	    result |= OBJ_COMMON;
	    break;
	default:
	    return(TRUE);
	    break;
    }
    
    p_elf_link_hash = (struct elf_link_hash_entry *)ld_slookup_mext(name,is_undef);

    if (p_elf_link_hash->def_regular) {
	result |= DEF_IN_OBJ;
    }
    if (p_elf_link_hash->ref_regular) {
	result |= USED_IN_OBJ;
    }
    if (p_elf_link_hash->ref_dynamic) {
	result |= USED_IN_DSO;
    }
    if (p_elf_link_hash->def_dynamic) {
	result |= DEF_IN_DSO;
    }
    if (p_elf_link_hash->hidden) {
    }


    if (p_elf_link_hash->ipa_indx != WHIRL_ST_IDX_UNINITIALIZED &&
    	p_elf_link_hash->ipa_indx != WHIRL_ST_IDX_NOT_AVAILABLE) {

    	(*p_Sync_symbol_attributes) (p_elf_link_hash->ipa_indx, 
				     result,
			    	     is_weak,
				     p_elf_link_hash->other);
	
    }
    
    return(TRUE);
}

static void
hash_dummy(struct bfd_link_hash_entry *p_bfd_link_hash, PTR info)
{

    if (p_bfd_link_hash->root.string) {
    	printf("*** %s\n",p_bfd_link_hash->root.string);
    }
}

	/*******************************************************
		Function: cleanup_symtab_for_ipa

		In -IPA mode, we pass the last bit of info in the
		merged symbol table that is needed by ipa.so, and
		then clean up the storage allcoated.
		
	 *******************************************************/
void
cleanup_symtab_for_ipa (void)
{

    /* first, synch. up the symbol attributes with IPA's WHIRL symtab */

    bfd_link_hash_traverse (link_info.hash, ipa_symbol_sync, (PTR) NULL);

    
}  /* cleanup_symtab_for_ipa */



	/*******************************************************
		Function: 

		

	 *******************************************************/







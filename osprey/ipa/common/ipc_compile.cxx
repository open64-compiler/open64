/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#include <stdint.h>
#include <limits.h>                     // for PATH_MAX
#include <unistd.h>                     // for read(2)/write(2)
#include <fcntl.h>                      // for open(2)
#include <sys/types.h>
#include <sys/stat.h>                   // for chmod(2)
#include <string.h>
#include <errno.h>                      // for sys_errlist
#include <vector>                       // for STL vector container

#include <ext/hash_map>                 // for STL hash_map container

#include <libgen.h>                     // for basename()
#include <time.h>                       // for time()
#include <sys/param.h>                  // for MAXPATHLEN

#include "linker.h"                     // std. linker's headers
#include "process.h"                    // for tmpdir, etc.
#include "main.h"                       // for arg_vectors
#include "ipc_weak.h"

#include "defs.h"                       // std. mongoose definitions
#include "errors.h"                     // for ErrMsg
#include "erglob.h"                     // error code
#include "glob.h"                       // for Tlog_File_Name
#include "tracing.h"                    // for Set_Trace_File
#include "cxx_memory.h"                 // for CXX_NEW
#include "opcode.h"                     // needed by wn_core.h
#include "wn_core.h"                    // needed by ir_bread.h
#include "pu_info.h"                    // needed by ir_bread.h
#include "ir_bread.h"                   // for WN_get_section_base ()

#include "dwarf_DST_mem.h"              // needed by ipc_file.h
#include "ipc_file.h"                   // for IP_FILE_HDR
#include "ipa_option.h"                 // ipa option flags
#include "ipc_link.h"                   // for ipa_link_link_argv

#include "lib_phase_dir.h"              // for BINDIR etc

#ifdef KEY
#include "ipc_defs.h"                   // for IPA_Target_Type
#endif

#pragma weak tos_string
#pragma weak outfilename

using std::vector;
using __gnu_cxx::hash_map;

#if defined(__linux__) || defined(BUILD_OS_DARWIN)
#define _USE_GNU_MAKE_
#endif

#ifdef _USE_GNU_MAKE_
#define TARGET_DELIMITER " : "
#else
#define TARGET_DELIMITER " ! "
#endif

#ifdef KEY
extern char *psclp_arg;
#endif

// To create an executable, we have to 
// -- Compile the symtab file to a .o file (an elf symtab)
// -- Compile the symtab to a .G file
// -- Using the .G file, compile each of the regular whirl files
//    (each file that contains a pu) to an elf .o file
// -- Link all of the .o files.

// For each of the regular whirl files, we keep track of 
// -- its name
// -- the command line we'll use to compile it
// -- a list of zero of more lines that will be added to the makefile
//    as comments.  Normally these will be the pu's contained in the file.

// How to use these functions:
// (1) Initialize with ipa_compile_init.
// (2) Call ipacom_process_symtab, where the argument is the symtab file
//     name.  (e.g. "symtab.I")
// (3) For each other whirl file:
//     (a) call ipacom_process_file
//     (b) call ipacom_add_comment zero or more times, using the 
//         index that ipacom_process_file returned.
// (4) call ipacom_doit.  ipacom_doit never returns.


// --------------------------------------------------------------------------
// File-local variables

// makefile_name is a full pathname, as is outfiles_fullpath, 
// all of the others are basename only.

static char* makefile_name = 0;         // name of the makefile
static FILE* makefile = 0; 

static vector<const char*>* infiles = 0;
static vector<const char*>* outfiles = 0;
static vector<const char*>* outfiles_fullpath = 0;
static vector<const char*>* commands = 0;
static vector<UINT32>* ProMP_Idx = 0;
static vector<vector<const char*> >* comments = 0;

// Name of the symtab file as written by the ipa.  (e.g. symtab.I)
static char input_symtab_name[PATH_MAX] = "";

// Name of the symtab file that will be used to compile other whirl
// files. (e.g. symtab.G)
static char whirl_symtab_name[PATH_MAX] = "";

// Name of the elf symtab file. (e.g. symtab.o)
static char elf_symtab_name[PATH_MAX] = "";

// Command line for producing the whirl and elf symtab files.  
static const char* symtab_command_line = 0;
static const char* symtab_extra_args = 0;


// Map from short forms of command names (e.g. "cc") to full 
// forms (e.g. "/usr/bin/cc").

namespace {
  struct eqstr {
    bool operator()(const char* s1, const char* s2) const
      { return strcmp(s1, s2) == 0; }
  };
  typedef __gnu_cxx::hash_map<const char*, const char*, __gnu_cxx::hash<const char*>, eqstr>
          COMMAND_MAP_TYPE;
}

static COMMAND_MAP_TYPE* command_map;

// --------------------------------------------------------------------------

// Overview of what happens post-ipa.  We create a makefile in the
// tmpdir.  We spawn a shell to invoke make.  Make will compile each
// of the .I files in tmpdir, and will then do the final link in the
// current working directory.  After make terminates, the shell
// deletes all of the in the tmp_file_list, including the makefile
// itself and the shell script itself, moves all of the other files in
// tmpdir into the current working directory, and delete the tmpdir.
// Upon interrupt (using sh's builtin trap command), it does the same
// thing.

static const char* get_extra_args(const char* ipaa_filename);
static const char* get_extra_symtab_args(const ARGV&);
static void exec_smake(char* cmdfile_name);

    /*
    	This is here because the gnu basename() doesn't strip
	off multiple slashes.
    */
static const char*ipa_basename(char *name){
    const char *bname = basename(name);
    
    while (*bname == '/')
    	bname++;
    return bname;
}

    /*
        This is here because the result from ipa_basename may
    contain illegal character '-', which should be changed to '_'
    */
static char* proper_name(char *name){
    for(char *i = name; *i; i++) {
      if ( !isalnum(*i) ) *i = '_';
    }
    return name;
}

static const char* abi()
{
#if defined(TARG_MIPS) || defined(TARG_LOONGSON)
    // 12343: Use IPA_Target_Type instead of ld_ipa_opt[LD_IPA_TARGOS].flag
    // to distinguish between n32 and 64 in IPA
    return IPA_Target_Type == IP_64_bit_ABI ? "-64" : "-n32";
#endif

#ifdef TARG_IA64
    return "-i64";
#endif

#ifdef TARG_IA32
    return "-i32";
#endif

#ifdef TARG_X8664
    return IPA_Target_Type == IP_64_bit_ABI ? "-m64" : "-m32";
#endif

  return "-n32";                // This line is never reached.
}

namespace {

// Returns true if path refers to an ordinary file.
bool file_exists(const char* path)
{
  if (!path || strlen(path) == 0)
    return false;

  struct stat buf;
  return stat(path, &buf) == 0 && S_ISREG(buf.st_mode);
}

} // Close unnamed namespace

#ifdef KEY
static inline bool
looks_like(const char *path, const char *base)
{
    int p = strlen(path), b = strlen(base);

    return (strcmp(path + p - b, base) == 0 &&
	    (path[p - b - 1] == '-' || path[p - b - 1] == '/'));
}
#endif

extern "C" void
ipa_compile_init ()
{ 
  Is_True(tmpdir, ("no IPA temp. directory"));

  Is_True(infiles == 0 && outfiles == 0 && commands == 0 && comments == 0
          && makefile_name == 0 && makefile == 0 && command_map == 0,
          ("ipa_compile_init already initialized"));

  infiles           = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  outfiles          = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  outfiles_fullpath = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  commands          = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  comments          = CXX_NEW (vector<vector<const char*> >, Malloc_Mem_Pool);

  if (infiles == 0 || outfiles == 0 || outfiles_fullpath == 0 ||
      commands == 0 || comments == 0)
    ErrMsg (EC_No_Mem, "ipa_compile_init");

  if (ProMP_Listing)
      ProMP_Idx = CXX_NEW (vector<UINT32>, Malloc_Mem_Pool);

  char name_buffer[256];
  sprintf(name_buffer, "makefile.ipa%ld", (long) getpid());

  makefile_name = create_unique_file(name_buffer, 0);
  add_to_tmp_file_list (makefile_name);

  makefile = fopen(makefile_name, "w");
  if (makefile == 0)
    ErrMsg (EC_Ipa_Open, makefile_name, strerror(errno));
  chmod(makefile_name, 0644);

  command_map = CXX_NEW(COMMAND_MAP_TYPE, Malloc_Mem_Pool);
  if (command_map == 0)
    ErrMsg (EC_No_Mem, "ipa_compile_init");

  const char* toolroot = getenv("TOOLROOT");

#if defined(TARG_IA64) || defined(TARG_X8664) || defined(TARG_MIPS) || defined(TARG_SL) || defined(TARG_LOONGSON)

  static char* smake_base = ALTBINPATH "/usr/bin/make";

#if defined(VENDOR_PSC)
#include "pathscale_defs.h"
  static const char* tmp_cc_name_base = PSC_INSTALL_PREFIX "/bin/" PSC_NAME_PREFIX "cc";
#elif defined(VENDOR_OSP)
  static const char* tmp_cc_name_base = "/usr/ia64-sgi-linux/bin/sgicc";
#else
  static const char* tmp_cc_name_base;
#endif
  static const char* cc_name_base = tmp_cc_name_base;
  static const char* cord_name_base= "/usr/bin/gen_cord";
  static char my_cc[MAXPATHLEN];
  char *where_am_i = getenv("COMPILER_BIN");
  int retval;

  if (where_am_i) {
    tmp_cc_name_base = where_am_i;
    cc_name_base = where_am_i;
  }

  if (my_cc[0] == '\0' &&
      (retval = readlink ("/proc/self/exe", my_cc, sizeof(my_cc))) >= 0) {

      my_cc[retval] = '\0';	// readlink doesn't append NULL

#if !defined(TARG_SL) && !defined(TARG_MIPS)
      if (looks_like (my_cc, OPEN64_NAME_PREFIX "cc") ||
	  looks_like (my_cc, OPEN64_NAME_PREFIX "CC") ||
	  looks_like (my_cc, OPEN64_NAME_PREFIX "f90")) {
#else
#define SLCC_NAME_PREFIX "sl"
      if (looks_like (my_cc, SLCC_NAME_PREFIX "cc") ||
	  looks_like (my_cc, SLCC_NAME_PREFIX "CC") ||
	  looks_like (my_cc, SLCC_NAME_PREFIX "f90")) {
#endif
	  tmp_cc_name_base = my_cc;
	  cc_name_base = my_cc;
      } else if (looks_like (my_cc, "ipa_link")) {
	  char *s = strrchr(my_cc, '/');
	  if (s) {
#ifndef TARG_SL
	      *s = '\0';		// remove "/ipa_link"
	      s = strrchr(my_cc, '/');
	      (s) ? *s = '\0' : 0;		// remove version number, e.g. "/2.3.99"
	      s = strrchr(my_cc, '/');
	      (s) ? *s = '\0' : 0;                // remove the 'targ_open64_linux'
	      s = strrchr(my_cc, '/');
	      (s) ? *s = '\0' : 0;                // remove the 'gcc-lib'
              s = strrchr(my_cc, '/');
#endif

	      if (s) {
		  // Invoke the C/C++/Fortran compiler depending on the source
		  // language.  Bug 8620.
		  const char *compiler_name_suffix;
		  if (!strcmp(IPA_lang, "F77") ||
		      !strcmp(IPA_lang, "F90")) {
		    compiler_name_suffix = "f95";
		  } else if (!strcmp(IPA_lang, "C")) {
		    compiler_name_suffix = "cc";
		  } else if (!strcmp(IPA_lang, "CC")) {
		    compiler_name_suffix = "CC";
		  } else {
		    Fail_FmtAssertion ("ipa: unknown language");
		  }
#if defined(VENDOR_PSC)
		  strcpy(++s, "bin/" PSC_NAME_PREFIX);
		  s += strlen("bin/" PSC_NAME_PREFIX);
#elif defined(VENDOR_SL)
		  strcpy(++s, SLCC_NAME_PREFIX);
		  s += strlen(SLCC_NAME_PREFIX);
#elif !defined(TARG_MIPS)
		  strcpy(++s, "bin/" OPEN64_NAME_PREFIX);
		  s += strlen("bin/" OPEN64_NAME_PREFIX);
#endif
		  strcpy(s, compiler_name_suffix);

		  if (file_exists (my_cc)) {
		      tmp_cc_name_base = my_cc;
		      cc_name_base = my_cc;
		  }
	      }
	  }
      }
  }
  

#define MAKE_STRING "make"

#else

  static char* smake_base = "/usr/sbin/smake";
  static char* tmp_cc_name_base = "/usr/bin/cc";
  static char* cc_name_base = "/usr/bin/cc";
  static char* cord_name_base= "/usr/bin/gen_cord";
#define MAKE_STRING "smake"

#endif

  (*command_map)["cord"] = cord_name_base;

  if (toolroot) {
      static char* new_cc_name_base = concat_names((const string)toolroot, (const string)tmp_cc_name_base);
      if (file_exists(new_cc_name_base))
	  (*command_map)["cc"] = new_cc_name_base;
      else 
	  (*command_map)["cc"] = cc_name_base;
  }
  else 
      (*command_map)["cc"]    = cc_name_base;

  // For smake we first try $TOOLROOT/usr/sbin/smake.  If that doesn't
  // exist then we try /usr/sbin/smake, and finally if *that* doesn't
  // exist we just use smake and hope that the user's search path 
  // contains something sensible.
  static const char* smake_name = 0;
  {
    if (toolroot != 0) {
      const char* tmp = concat_names((const string)toolroot, (const string)smake_base);
      if (file_exists(tmp))
        smake_name = tmp;
    }

    if (smake_name == 0) {
      if (file_exists(smake_base))
        smake_name = smake_base;
      else
        smake_name = MAKE_STRING;
    }
  }

  (*command_map)[MAKE_STRING] = smake_name;

#ifdef TODO
  if (IPA_Enable_Cord) {
    cord_output_file_name = create_tmp_file ("cord_script");
    call_graph_file_name = create_tmp_file ("ipa_cg");
    add_to_tmp_file_list (call_graph_file_name);
    Call_graph_file = FOPEN (call_graph_file_name, "w");
    if (Call_graph_file == 0)
#ifdef TARG_IA64
      {
      perror(call_graph_file_name);
      exit(1);
      }
#else
      msg (ER_FATAL, ERN_IO_FATAL, call_graph_file_name,
           strerror(errno)); 
#endif
  }
#else
  DevWarn ("TODO: support ipa-cord");
#endif
} // ipa_compile_init


// Generate a command line to compile an ordinary whirl file into an object
// file.
static void
get_command_line (const IP_FILE_HDR& hdr, ARGV& argv, const char* inpath,
                  const char* outpath) 
{
  char* base_addr = (char*)
    WN_get_section_base (IP_FILE_HDR_input_map_addr (hdr), WT_COMP_FLAGS);

  if (base_addr == (char*) -1)
    ErrMsg (EC_IR_Scn_Read, "command line", IP_FILE_HDR_file_name (hdr));

  Elf64_Word argc = *((Elf64_Word *) base_addr);
  Elf64_Word* args = (Elf64_Word *) (base_addr + sizeof(Elf64_Word));

  // args[0] is the command, so we need to treat it specially.  If
  // TOOLROOT is set, and if args[0] isn't already an absolute pathname,
  // we need to construct an absolute pathname using TOOLROOT.

  Is_True(command_map != 0
            && command_map->find("cc") != command_map->end()
            && (*command_map)["cc"] != 0
            && strlen((*command_map)["cc"]) != 0,
          ("Full pathname for cc not set up"));

  if (argc > 0) {
    argv.push_back((*command_map)["cc"]);

    for (INT i = 1; i < argc; ++i) {
      argv.push_back (base_addr + args[i]);
    }
  }
  else {
    argv.push_back ((*command_map)["cc"]);
    argv.push_back ("-c");
  }

  argv.push_back(abi());

  argv.push_back (inpath);
  argv.push_back ("-o");
  argv.push_back (outpath);
  argv.push_back ("-c");

  if (ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag)
    argv.push_back ("-keep");
    
} // get_command_line


// temp. kludge:  ipacom_process_file should really take
// const IP_FILE_HDR& as argument instead of const PU_Info *
#include "ipc_symtab_merge.h"
static const IP_FILE_HDR&
get_ip_file_hdr (const PU_Info *pu)
{
  ST_IDX st_idx = PU_Info_proc_sym (pu);
  PU_IDX pu_idx = ST_pu (St_Table[st_idx]);
  return *AUX_PU_file_hdr (Aux_Pu_Table[pu_idx]);
}

extern "C" void
ipacom_process_symtab (char* symtab_file)
{

  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0,
          ("ipacom_process_symtab: ipacom not yet initialized"));

  Is_True(strlen(input_symtab_name) == 0 &&
          strlen(whirl_symtab_name) == 0 &&
          strlen(elf_symtab_name) == 0 &&
          symtab_command_line == 0,
          ("ipacom_process_symtab: symtab already initialized"));

  char* output_file = create_unique_file (symtab_file, 'o');
  add_to_tmp_file_list (output_file);
#ifdef _USE_GNU_MAKE_
  unlink (output_file);
#endif

  const char* input_base = ipa_basename(symtab_file);
  const char* output_base = ipa_basename(output_file);

  // Save the three symtab file names in global variables.
  strcpy(input_symtab_name, input_base);
  strcpy(elf_symtab_name,   output_base);
  strcpy(whirl_symtab_name, output_base);
  whirl_symtab_name[strlen(whirl_symtab_name) - 1] = 'G';

  // Generate a command line to create the .G and .o files.
  char buf[3*PATH_MAX + 64];

  Is_True(command_map != 0
            && command_map->find("cc") != command_map->end()
            && (*command_map)["cc"] != 0
            && strlen((*command_map)["cc"]) != 0,
          ("Full pathname for cc not set up"));

  char* toolroot = getenv("TOOLROOT");

#if defined(VENDOR_OSP) || defined(VENDOR_SL)
  sprintf(buf, "%s -c %s %s -o %s %s -TENV:emit_global_data=%s %s",
#else
  sprintf(buf, "%s%s -c %s %s -o %s %s -TENV:emit_global_data=%s %s",
	    (toolroot != 0) ? toolroot : "",
#endif
            (*command_map)["cc"],
            abi(),
            input_symtab_name,
            elf_symtab_name,
            ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag ? "-keep":"",
            whirl_symtab_name,
            IPA_Enable_AutoGnum?"-Gspace 0":"");


  char* cmd = static_cast<char*>(malloc(strlen(buf) + 1));
  if (!cmd)
    ErrMsg (EC_No_Mem, "ipacom_process_symtab");

  strcpy(cmd, buf);
  symtab_command_line = cmd;

  Is_True(strlen(input_symtab_name) != 0 &&
          strlen(whirl_symtab_name) != 0 &&
          strlen(elf_symtab_name) != 0 &&
          symtab_command_line != 0,
          ("ipacom_process_symtab: initialization failed"));

} // ipacom_process_symtab

// The return value is the index of this file in the vectors.
extern "C" 
size_t ipacom_process_file (char* input_file,
                            const PU_Info* pu, UINT32 ProMP_id)
{
  Is_True(infiles != 0 && outfiles_fullpath != 0 && commands != 0 &&
          comments != 0,
          ("ipacom_process_file: ipacom not initialized"));

  Is_True(strlen(input_symtab_name) != 0 &&
          strlen(whirl_symtab_name) != 0 &&
          strlen(elf_symtab_name) != 0 &&
          symtab_command_line != 0,
          ("ipacom_process_file: symtab not initialized"));

  if (ProMP_Listing) {
      Is_True (ProMP_Idx != 0,
	       ("ipacom_process_file: ipacom not initialized"));
      ProMP_Idx->push_back (ProMP_id);
  }

  char* output_file = create_unique_file (input_file, 'o');

  add_to_tmp_file_list (output_file);

  const char* input_base = ipa_basename (input_file);
  const char* output_base = ipa_basename (output_file);

  infiles->push_back(input_base);
  outfiles->push_back(output_base);
  outfiles_fullpath->push_back(output_file);

  // Assemble the command line.

  ARGV argv;                          // vector<const char*>
  get_command_line (get_ip_file_hdr (pu), argv, input_base, output_base);

  char* str = (char*) malloc(2 * PATH_MAX + 64);
  sprintf(str, "-TENV:ipa_ident=%ld -TENV:read_global_data=%s %s",
          time(0),
          whirl_symtab_name,
          IPA_Enable_AutoGnum?"-Gspace 0":"");


  argv.push_back(str);

#ifdef KEY
  // Add "-psclp filename".
  if (psclp_arg != NULL) {
    char* str = static_cast<char*>(strdup(psclp_arg));
    argv.push_back(str);
  }
#endif

  if (ProMP_Listing) {
    char* str = static_cast<char*>(malloc(64));
    sprintf(str, "-PROMP:=ON -PROMP:next_id=%lu", (unsigned long) ProMP_id);
    argv.push_back(str);
  }
    
  //char* gspace = (char*) malloc(2 * PATH_MAX + 64);
  //strcpy(gspace, "-Gspace 0");
  //argv.push_back(gspace);

#ifdef TODO
  if (gspace_size) {
    WRITE_STRING("-Gspace", argv->argv[i]);
    sprintf(str, "%d", gspace_size);
    WRITE_STRING(str, argv->argv[++i]);
  }
#else
  static bool reported = false;
  if (!reported) {
    reported = true;
    DevWarn ("TODO: implement gspace_size command file");
  }
  if (IPA_Enable_Array_Sections)
    argv.push_back("-LNO:ipa");
#endif

  // Piece the command line together and push it onto the list.
  size_t cmdline_length = 0;
  ARGV::const_iterator i;

  for (i = argv.begin(); i != argv.end(); ++i)
    cmdline_length += strlen(*i) + 1;

  char* cmdline = static_cast<char*>(malloc(cmdline_length + 1));
  if (!cmdline)
    ErrMsg (EC_No_Mem, "ipacom_process_file");    

  cmdline[0] = '\0';
    
  for (i = argv.begin(); i != argv.end(); ++i) {
    strcat(cmdline, *i);
    strcat(cmdline, " ");
  }

  commands->push_back(cmdline);
    
  // Add an empty vector for this file's comments.
#ifdef KEY // porting to GNU 3.*
  vector<const char*>* emptyvector = CXX_NEW(vector<const char*>, 
					     Malloc_Mem_Pool);
  comments->push_back(*emptyvector);
#else
  comments->push_back();
#endif

  Is_True (infiles->size() > 0 &&
           infiles->size() == outfiles->size() &&
           infiles->size() == outfiles_fullpath->size() &&
           infiles->size() == commands->size() &&
           infiles->size() == comments->size(),
           ("ipacom_process_file: inconsistent vector sizes"));

  // Set up extra args for compiling symtab, if necessary.
  if (!symtab_extra_args)
    symtab_extra_args = get_extra_symtab_args(argv);

  return infiles->size() - 1;

} // ipacom_process_file

// Each file has a list of zero or more comments that will appear in the
// makefile.  (Usually, each comment will be the name of a pu.)  
// This function adds a comment to the n'th file's list.
extern "C"
void ipacom_add_comment(size_t n, const char* comment)
{
  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0,
          ("ipacom_add_comment: ipacom not initialized"));

  Is_True(comments->size() >= n + 1,
          ("ipacom_add_comment: invalid index %ld, max is %ld",
           n, comments->size()));

  Is_True(comment != 0, ("ipacom_add_comment: argument is a null pointer"));

  char* tmp = static_cast<char*>(malloc(strlen(comment) + 1));
  if (!tmp)
    ErrMsg (EC_No_Mem, "ipacom_add_commend");
  strcpy(tmp, comment);

  (*comments)[n].push_back(tmp);
}

namespace {

char* ipc_copy_of (char *str)
{
  register int len;
  register char *p;

  len = strlen(str) + 1;
  p = (char *) MALLOC (len);
  MALLOC_ASSERT (p);
  BCOPY (str, p, len);
  return p;
} /* ipc_copy_of */

void print_obj_listfiles(const char* dirname, FILE* listfile)
{
 
  for (vector<const char*>::iterator i = outfiles->begin();
       i != outfiles->end();
       ++i)
    fprintf(listfile, "%s/%s \n", dirname, *i);
  
  if (strlen(elf_symtab_name) != 0)
    fprintf(listfile, "%s/%s \n", dirname, elf_symtab_name);
}

void print_all_outfiles(const char* dirname)
{
 
  for (vector<const char*>::iterator i = outfiles->begin();
       i != outfiles->end();
       ++i)
    fprintf(makefile, "%s%s/%s \\\n", "   ", dirname, *i);
  
  if (strlen(elf_symtab_name) != 0)
    fprintf(makefile, "%s%s/%s \n", "   ", dirname, elf_symtab_name);
}

} // Close unnamed namespace

static 
const char*
Get_Annotation_Filename_With_Path (void) {
    static char buf[MAXPATHLEN];

    if (!Annotation_Filename) { buf[0] = '\0'; }
    else if (*Annotation_Filename == '/') {
        strcpy (buf, Annotation_Filename);
    }else {
#ifdef KEY
        strcpy (buf, "$$dir/");		// bug 11686
#else
        strcpy (buf, "../");
#endif
        strcat (buf, Annotation_Filename);
    }
     
    return &buf[0];
}

extern "C"
void ipacom_doit (const char* ipaa_filename)
{
  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0 && makefile != 0,
          ("ipacom_doit: ipacom not yet initialized"));
  Is_True(infiles->size() == outfiles->size() &&
          infiles->size() == outfiles_fullpath->size() &&
          infiles->size() == commands->size() &&
          infiles->size() == comments->size(),
          ("ipacom_doit: vectors are inconsistent"));


  if (infiles->size() > 0) {
    Is_True(strlen(input_symtab_name) != 0 &&
            strlen(whirl_symtab_name) != 0 &&
            strlen(elf_symtab_name) != 0 &&
            symtab_command_line != 0,
            ("ipacom_doit: symtab not initialized"));
  }

#ifdef TODO
  if (IPA_Enable_Cord) {
    FCLOSE (Call_graph_file);
    if (IPA_Enable_final_link)
      process_cord_cmd ();
  }
#endif

  // These are used when compiling each .I file.
  const char* extra_args = get_extra_args(ipaa_filename);

  const char* tmpdir_macro_name = "IPA_TMPDIR";
  const char* tmpdir_macro      = "$(IPA_TMPDIR)";
  fprintf(makefile, "%s = %s\n\n", tmpdir_macro_name, tmpdir);

  char* link_cmdfile_name = 0;

  // The default target: either the executable, or all of the
  // elf object files.

  if (IPA_Enable_final_link) {
    // Path (possibly relative to cwd) of the executable we're creating.
    const char* executable = outfilename;
    const char* executable_macro_name = "IPA_OUTFILENAME";
    const char* executable_macro      = "$(IPA_OUTFILENAME)";

    fprintf(makefile, "%s = %s\n\n", executable_macro_name, executable);
    fprintf(makefile, ".PHONY: default\n");
    fprintf(makefile, "default: %s\n\n", executable_macro);
#ifdef KEY
    // bug 2487
    // bug 3594: emit backslash if there is only symtab.o
    fprintf(makefile, "%s%s%s\n", executable_macro, TARGET_DELIMITER,
	    outfiles->size() || strlen(elf_symtab_name) ? "\\" : "");
#else
    fprintf(makefile, "%s%s\\\n", executable_macro, TARGET_DELIMITER);
#endif

#ifdef TODO
    if (IPA_Enable_Cord) 
    	fprintf(makefile, "%s%s \\\n", "   ",  cord_output_file_name);
#endif

    print_all_outfiles(tmpdir_macro);


    // The final link command is just ld -from <cmdfile>.  Everything else
    // goes into cmdfile.

    // Create a temporary file for cmdfile.
    char cmdfile_buf[256];
    sprintf(cmdfile_buf, "linkopt.%ld", (long) getpid());
    link_cmdfile_name = create_unique_file(cmdfile_buf, 0);
    FILE* cmdfile = fopen(link_cmdfile_name, "w");
    if (cmdfile == 0)
      ErrMsg (EC_Ipa_Open, link_cmdfile_name, strerror(errno));
    chmod(link_cmdfile_name, 0644);
    
    // Get the link command line.
    const ARGV* link_line = ipa_link_line_argv (outfiles_fullpath, 
    	    	    	    	    	    	tmpdir, 
						elf_symtab_name);
    Is_True(link_line->size() > 1, ("Invalid link line ARGV vector"));

    // Print all but link_line[0] into cmdfile.
    ARGV::const_iterator i = link_line->begin();

    // see whether we use ld/collect or gcc/g++ to link objects, if
    // we use gcc/g++ to link the object, we should ignore the CRTs. 
    const char* linker = strrchr(*i, '/');  
    BOOL no_crt = TRUE; 
    if (linker && (!strcmp (linker, "/ld") || !strcmp (linker, "/collect")) ||
        !linker && (!strcmp (*i, "ld") || !strcmp (*i, "collect"))) {
       no_crt = FALSE;
    }

    for (++i; i != link_line->end(); ++i) {
#ifdef KEY
      // Since we are using GCC to link, don't print out the run-time support
      // files.
      const char *p;
#ifndef TARG_SL // jczhang: use slcc specific crt*.o
      if (((p = strstr(*i, "/crt1.o")) && p[7] == '\0') ||
          ((p = strstr(*i, "/Scrt1.o")) && p[8] == '\0') ||
	  ((p = strstr(*i, "/crti.o")) && p[7] == '\0') ||
	  ((p = strstr(*i, "/crtbegin.o")) && p[11] == '\0') ||
          ((p = strstr(*i, "/crtbeginS.o")) && p[12] == '\0') ||
	  ((p = strstr(*i, "/crtend.o")) && p[9] == '\0') ||
          ((p = strstr(*i, "/crtendS.o")) && p[10] == '\0') ||
	  ((p = strstr(*i, "/crtn.o")) && p[7] == '\0')) {
	continue;
      }
#endif
#endif
      // Since we're using gcc to link, we must mangle linker
      // directives that we know about so they are acceptable to it,
      // and passed properly to its linker.
      if (strcmp(*i, "-rpath") == 0) {
	fputs("-Wl,-rpath,", cmdfile);
	++i;
      }
      if (strcmp(*i, "-rpath-link") == 0) {
        fputs("-Wl,-rpath-link,", cmdfile);
        ++i;
      }
      if (strcmp(*i, "-whole-archive") == 0) {
        fputs("-Wl,-whole-archive", cmdfile);
        fputs(" \n", cmdfile);
        continue;
      }
      if (strcmp(*i, "-no-whole-archive") == 0) {
        fputs("-Wl,-no-whole-archive", cmdfile);
        fputs(" \n", cmdfile);
        continue;
      }
      if (strncmp(*i, "-soname=", 8) == 0) {
        fputs("-Wl,", cmdfile);
        fputs(*i, cmdfile);
        fputs(" \n", cmdfile);
        continue;
      }
      fputs(*i, cmdfile);
      fputs(" \n", cmdfile);
    }

#ifdef TODO
    if (IPA_Enable_Cord) {
      fputs("-T", cmdfile);
      fprintf(cmdfile, " %s\n", cord_output_file_name);
    }
#endif

    fputs("\n", cmdfile);
    fclose(cmdfile);

    // If we're compiling with -show, make sure we see the link line.
    if (ld_ipa_opt[LD_IPA_SHOW].flag) {
      fprintf(makefile, "\techo -n %s ' ' ; cat %s\n",
              link_line->front(), link_cmdfile_name);
#ifndef _USE_GNU_MAKE_
      fprintf(makefile, "\t...\n");
#endif
    }

    // Print the final link command into the makefile.
#if defined(TARG_IA64) || defined(TARG_X8664) || defined(TARG_MIPS) || defined(TARG_SL)
    // Create a dir in /tmp and create symbolic links inside it to point to the
    // IPA .o files in the IPA tmpdir.  In the link command file, refer to
    // these symbolic links instead of the IPA tmpdir .o files, in order to
    // shorten the args in the link command file.  For example,
    // /home/blah/very/long/path/1.o becomes /tmp/symlinksdir/1.o.  Fixes bugs
    // 5876 (very long path), and 7801/7866 (must link in current dir).

    // Create symbolic links dir in /tmp.
    const char *outfile_basename = ipa_basename(outfilename);
    char *symlinksdir = (char *) alloca(20 + strlen(outfile_basename));
    sprintf(symlinksdir, "/tmp/%s.ipaXXXXXX", outfile_basename);
    symlinksdir = mktemp(symlinksdir);

    // In the makefile, set up the symbolic links and modify the link command
    // to reference these links:
    //   mkdir symlinksdir
    //   for i in `grep ^tmpdir/.\*.o link_cmdfile`; do
    //     ln -s $i symlinksdir
    //   done
    //   gcc `sed 's:tmpdir:symlinksdir:' link_cmdfile`
    //   rm -r symlinksdir

    fprintf(makefile, "\tmkdir %s\n", symlinksdir);
    fprintf(makefile, "\td=`pwd` ; \\\n");
    fprintf(makefile, "\tfor i in `grep ^%s/.\\*.o %s`; do ln -s %s$$i %s; done\n",
	    tmpdir, link_cmdfile_name,
	    tmpdir[0] == '/' ? "" : "$$d/",
	    symlinksdir);
#ifdef TARG_SL //jczhang: link with SL's ld instead of gcc
    char *toolroot = getenv("TOOLROOT");
    fprintf(makefile, "\t%s%s `sed 's:%s:%s:' %s`\n",
	    toolroot, BINPATH"/ld", tmpdir, symlinksdir, link_cmdfile_name);
#else
    fprintf(makefile, "\t%s `sed 's:%s:%s:' %s`\n",
	    link_line->front(),
	    tmpdir, symlinksdir, link_cmdfile_name);
#endif // TARG_SL
    fprintf(makefile, "\trm -r %s\n", symlinksdir);
#elif defined(TARG_LOONGSON)
    fprintf(makefile, "\t%s `cat %s `\n",
            link_line->front(),
            link_cmdfile_name);
#else
    fprintf(makefile, "\t%s -from %s\n",
            link_line->front(),
            link_cmdfile_name);
#endif

    //For ProMP we need to run a Perl script after doing the final link.
    if (ProMP_Listing) {
      const char* toolroot = getenv("TOOLROOT");
      static const char* script_base = "/usr/lib32/cmplrs/pfa_reshuffle";
      const char* script_name = toolroot ? concat_names((const string)toolroot, (const string)script_base)
                                         : script_base;
      struct stat dummy;
      if (stat("/bin/perl5", &dummy) == 0 && stat(script_name, &dummy) == 0) {
        fprintf(makefile, "\t/bin/perl5 %s", script_name);
        vector<const char*>::const_iterator i = outfiles_fullpath->begin();
        for ( ; i != outfiles_fullpath->end(); ++i)
          fprintf(makefile, " %s", *i);
        fprintf(makefile, "\n");
      }
      else {
        if (stat("/bin/perl5", &dummy) != 0) {
          DevWarn("Can't find perl5 to run the ProMP reshuffle script");
        }
        if (stat(script_name, &dummy) != 0) {
          DevWarn("Can't find the ProMP reshuffle script");
        }
      }

      // For ProMP we also need to concatenate all of the .list files into
      // a file in the same directory as the excecutable.  The file is
      // named <executable>.list
      fprintf(makefile, "\tif [ -f %s.list ] ; then 'rm' -f %s.list ; fi\n",
              executable_macro, executable_macro);
      fprintf(makefile, "\t'cat' %s/*.list > %s.list\n",
              tmpdir_macro, executable_macro);
    }

    // Do the same thing with .t and .tlog files (if they exist) as
    // with .list files.
    bool tlogs_enabled = Get_Trace(TP_PTRACE1, 0xffffffff) ||
                         Get_Trace(TP_PTRACE2, 0xffffffff);
    bool t_enabled = TFile != stdout;

    if (tlogs_enabled) {
      fprintf(makefile, "\tif 'ls' -f %s/*.tlog > /dev/null 2>&1 ; then ",
              tmpdir);
      // Beforehand the output of cat was appended to
      // <executable>.<log>, but this requires the user to remove
      // <executable>.t before invocation when there is no
      // <executable>.<suffix> source file.  But we don't want to just
      // truncate <executable>.<log> before writing, since
      // <executable>.<log> might be created during input WHIRL
      // generation.  So we truncate and write to <tmpdir>.log
      // instead.
      fprintf(makefile, "'cat' %s/*.tlog > %s.tlog ; true ; fi\n",
              tmpdir, tmpdir);
    }
    if (t_enabled) {
      fprintf(makefile, "\tif 'ls' -f %s/*.t > /dev/null 2>&1 ; then ",
              tmpdir);
      fprintf(makefile, "'cat' %s/*.t > %s.t ; true ; fi\n",
              tmpdir, tmpdir);
    }
  }
  else {
    fprintf(makefile, ".PHONY: default\n");
    fprintf(makefile, "\ndefault: \\\n");      
    print_all_outfiles(tmpdir_macro);
  }

  fputs("\n", makefile);

  // This generates both the .o symtab and the .G symtab.
  if (strlen(elf_symtab_name) != 0) {
    char* toolroot = getenv("TOOLROOT");
    Is_True(strlen(input_symtab_name) != 0 &&
            strlen(whirl_symtab_name) != 0 &&
            symtab_command_line != 0 && strlen(symtab_command_line) != 0,
            ("ipacom_doit: symtab not initialized"));

    if (!symtab_extra_args)
      symtab_extra_args = get_extra_args(0);

#if defined(TARG_IA64) || defined(TARG_X8664) || defined(TARG_MIPS) || defined(TARG_SL) || defined(TARG_LOONGSON)

    if (IPA_Enable_Cord) {
    	const char * obj_listfile_name = create_tmp_file((const string)"obj_file_list");
    	FILE* listfile = fopen(obj_listfile_name, "w");
	print_obj_listfiles(tmpdir, listfile);
        fclose(listfile);
	
    	fprintf(makefile, "%s%s \\\n", cord_output_file_name, TARGET_DELIMITER);
    	print_all_outfiles(tmpdir_macro);
    	fprintf(makefile, "\t%s%s -o %s %s %s\n",(toolroot != 0) ? toolroot : "",(*command_map)["cord"], cord_output_file_name, call_graph_file_name, obj_listfile_name);//gen_cord
    }
	    

    fprintf(makefile, "%s/%s" TARGET_DELIMITER "\n",
            tmpdir_macro, "dummy");

    char *tmpname = (char *) malloc (strlen (outfilename) + 1);
    strcpy (tmpname, outfilename);
    if (Feedback_Filename) {
            fprintf(makefile, "\tcd %s; %s -Wb,-OPT:procedure_reorder=on -fb_create %s %s -Wb,-CG:enable_feedback=off -TENV:object_name=_%s\n\n",
                tmpdir_macro, symtab_command_line, Feedback_Filename, symtab_extra_args, proper_name((const string)ipa_basename(tmpname)));
    } else if (Annotation_Filename) {
      fprintf (makefile, "\t"
#ifdef KEY
	       "dir=`pwd`; "	// for calculating feedback prefix
#endif
	       "cd %s; %s -Wb,-OPT:procedure_reorder=on -fb_opt %s %s -TENV:object_name=_%s "
#ifdef KEY
	       "-Wb,-CG:enable_feedback=on\n\n",  // enable feedback for cg
#else
	       "-Wb,-CG:enable_feedback=off\n\n",
#endif
	       tmpdir_macro, symtab_command_line, 
	       Get_Annotation_Filename_With_Path (),
	       symtab_extra_args, proper_name((const string)ipa_basename(tmpname)));
    } else {
             fprintf(makefile, "\tcd %s; %s -Wb,-OPT:procedure_reorder=on %s -Wb,-CG:enable_feedback=off -TENV:object_name=_%s\n\n",
                     tmpdir_macro, symtab_command_line, symtab_extra_args, proper_name((const string)ipa_basename(tmpname)));
    }                                                                                                                    
    fprintf(makefile, "%s/%s" TARGET_DELIMITER "%s/%s %s/%s\n\n",
            tmpdir_macro, elf_symtab_name,
            tmpdir_macro, input_symtab_name,
            tmpdir_macro, "dummy");

    fprintf(makefile, "%s/%s" TARGET_DELIMITER "%s/%s %s/%s\n\n",
            tmpdir_macro, whirl_symtab_name,
            tmpdir_macro, elf_symtab_name,
            tmpdir_macro, "dummy");
#endif

#ifdef _TARG_MIPS

    fprintf(makefile, "%s/%s" TARGET_DELIMITER "%s/%s\n",
            tmpdir_macro, elf_symtab_name,
            tmpdir_macro, input_symtab_name);

    fprintf(makefile, "\tcd -P %s; %s %s\n\n",
            tmpdir_macro, symtab_command_line, symtab_extra_args);
      
    fprintf(makefile, "%s/%s" TARGET_DELIMITER "%s/%s\n\n",
            tmpdir_macro, whirl_symtab_name,
            tmpdir_macro, elf_symtab_name);

#endif

  }

  // For each whirl file, tell how to create the corresponding elf file.
  for (size_t i = 0; i < infiles->size(); ++i) {
    fprintf(makefile, "%s/%s" TARGET_DELIMITER "%s/%s %s/%s %s/%s\n",
            tmpdir_macro, (*outfiles)[i],
            tmpdir_macro, elf_symtab_name,
            tmpdir_macro, whirl_symtab_name,
            tmpdir_macro, (*infiles)[i]);
#if defined(TARG_IA64) || defined(TARG_X8664) || defined(TARG_MIPS) || defined(TARG_SL) || defined(TARG_LOONGSON)
    if (Feedback_Filename) {
        fprintf(makefile, "\tcd %s; %s -Wb,-OPT:procedure_reorder=on -fb_create %s %s -Wb,-CG:enable_feedback=off\n",
                tmpdir_macro, (*commands)[i], Feedback_Filename, extra_args);
    } else if (Annotation_Filename) {
      fprintf(makefile, "\t"
#ifdef KEY
	      "dir=`pwd`; "
#endif
	      "cd %s; %s -Wb,-OPT:procedure_reorder=on -fb_opt %s %s "
#ifdef KEY
	      "-Wb,-CG:enable_feedback=on\n",	// enable feedback for cg
#else
	      "-Wb,-CG:enable_feedback=off\n",
#endif
	      tmpdir_macro, (*commands)[i], 
	      Get_Annotation_Filename_With_Path () , extra_args);
    } else {
        fprintf(makefile, "\tcd %s; %s -Wb,-OPT:procedure_reorder=on %s -Wb,-CG:enable_feedback=off\n",
                tmpdir_macro, (*commands)[i], extra_args);
    }                                                                                                                    
#else
    if (Feedback_Filename) {
        fprintf(makefile, "\tcd %s; %s -Wb,-OPT:procedure_reorder=on -fb_create %s %s -Wb,-CG:enable_feedback=off\n",
                tmpdir_macro, (*commands)[i], Feedback_Filename, extra_args);
    } else if (Annotation_Filename) {
        fprintf(makefile, "\tcd %s; "
#ifdef KEY
		"dir=`pwd`; "
#endif
		"%s -Wb,-OPT:procedure_reorder=on -fb_opt %s %s -Wb,-CG:enable_feedback=off \n",
                tmpdir_macro, (*commands)[i], 
                Get_Annotation_Filename_With_Path (),extra_args);
    } else {
        fprintf(makefile, "\tcd -P %s; %s -Wb,-OPT:procedure_reorder=on %s -Wb,-CG:enable_feedback=off\n",
                tmpdir_macro, (*commands)[i], extra_args);
    }                                                                                                                    
#endif

    const vector<const char*>& com = (*comments)[i];
    for (vector<const char*>::const_iterator it = com.begin();
         it != com.end();
         ++it)
      fprintf(makefile, "## %s\n", *it);
    fputs("\n", makefile);
  }

  fclose(makefile);
  if (Tlog_File_Name) {
    fclose (Tlog_File);  
  }

  // We don't call make directly.  Instead we call sh, and have it
  // call sh.  This makes cleanup simpler.
  char sh_cmdfile_buf[256];
  sprintf(sh_cmdfile_buf, "cmdfile.%ld", (long) getpid());
  char* sh_cmdfile_name = create_unique_file(sh_cmdfile_buf, 0);

  FILE* sh_cmdfile = fopen(sh_cmdfile_name, "w");
  if (sh_cmdfile == 0)
    ErrMsg (EC_Ipa_Open, sh_cmdfile_name, strerror(errno));
  chmod(sh_cmdfile_name, 0644);  

  // Define a shell function for the cleanup.
  if (!ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag) {
    fprintf(sh_cmdfile, "cleanup() {\n");
    // Remove each temporary file that we know about.
    vector<const char*>::iterator i;
    for (i = infiles->begin(); i != infiles->end(); ++i)
      fprintf(sh_cmdfile, "if [ -f %s/%s ] ; then 'rm' -f %s/%s ; fi\n",
              tmpdir, *i, tmpdir, *i);
    for (i = outfiles->begin(); i != outfiles->end(); ++i)
      fprintf(sh_cmdfile, "if [ -f %s/%s ] ; then 'rm' -f %s/%s ; fi\n",
              tmpdir, *i, tmpdir, *i);

    if (strlen(input_symtab_name) != 0) {
      Is_True(strlen(whirl_symtab_name) != 0 && strlen(elf_symtab_name) != 0,
              ("Inconsistent symtab names: input, whirl, elf = %d %d %d\n",
               strlen(input_symtab_name),
               strlen(whirl_symtab_name),
               strlen(elf_symtab_name)));      
    
      fprintf(sh_cmdfile, "if [ -f %s/%s ] ; then 'rm' -f %s/%s ; fi\n",
              tmpdir, input_symtab_name, tmpdir, input_symtab_name);
      fprintf(sh_cmdfile, "if [ -f %s/%s ] ; then 'rm' -f %s/%s ; fi\n",
              tmpdir, whirl_symtab_name, tmpdir, whirl_symtab_name);
      fprintf(sh_cmdfile, "if [ -f %s/%s ] ; then 'rm' -f %s/%s ; fi\n",
              tmpdir, elf_symtab_name, tmpdir, elf_symtab_name);
    }

    if (link_cmdfile_name)
      fprintf(sh_cmdfile, "if [ -f %s ] ; then 'rm' -f %s ; fi\n",
              link_cmdfile_name, link_cmdfile_name);

    fprintf(sh_cmdfile,
            "'rm' %s > /dev/null 2>&1 ; true\n", makefile_name);
#ifndef KEY
    fprintf(sh_cmdfile,
            "'rm' %s > /dev/null 2>&1 ; true\n", sh_cmdfile_name);
#endif
    // Move any files that we don't know about to cwd.  We use a 
    // complicated shell command because, if no such files exist, we
    // don't want the user to see any diagnostics.
    fprintf(sh_cmdfile, "'mv' %s/* . > /dev/null 2>&1 ; true\n", tmpdir);

    // Remove the directory.
    fprintf(sh_cmdfile, "'rm' -rf %s > /dev/null 2>&1 ; true\n", tmpdir);
#ifdef KEY
    // fix for bug 254
    fprintf(sh_cmdfile,
            "'rm' %s > /dev/null 2>&1 ; true\n", sh_cmdfile_buf);
#endif
    fprintf(sh_cmdfile, "}\n\n");   // End of cleanup function.

    // Establish a signal handler so that cleanup always gets called.
    // SEGV should be here too; we're leaving it out because 6.2 sh doesn't
    // like it.
    fprintf(sh_cmdfile, "trap 'cleanup; exit 2' ");
#if !defined(TARG_IA64) && !defined(TARG_X8664) && !defined(TARG_MIPS) && !defined(TARG_SL) && !defined(TARG_LOONGSON)
    fprintf(sh_cmdfile, "ABRT EMT SYS POLL ");
#endif
    fprintf(sh_cmdfile, "HUP INT QUIT ILL TRAP FPE ");
    fprintf(sh_cmdfile, "KILL BUS PIPE ALRM TERM ");
    fprintf(sh_cmdfile, "USR1 USR2 IO VTALRM PROF XCPU XFSZ\n\n\n");
  }

    // ensure MAKEFLAGS is not passed to smake(or make).
    // MAKEFLAGS can only cause trouble if passed in,
    // and gmake passes CCTYPE=  information into
    // MAKEFLAGS.  A problem 
    // for smake if CCTYPE=-Ofast is used, as the 't'
    // is interpreted as the MAKEFLAGS 'touch only'
    // option of smake.
    // Using the simplest, most universal env var reset
    // command format, that of plain old sh.
  fprintf(sh_cmdfile,"#! /bin/sh -f \n");
  fprintf(sh_cmdfile,"MAKEFLAGS=\nexport MAKEFLAGS\n");

  // Call smake.
  Is_True(command_map != 0
          && command_map->find(MAKE_STRING) != command_map->end()
          && (*command_map)[MAKE_STRING] != 0
          && strlen((*command_map)[MAKE_STRING]) != 0,
          ("Full pathname for smake not set up"));
  const char* smake_name = (*command_map)[MAKE_STRING];
#ifdef KEY	// bug 2487
  fprintf(sh_cmdfile, "rm -f %s\n", outfilename);
#endif
  fprintf(sh_cmdfile, "%s -f %s ", smake_name, makefile_name);

  if (!ld_ipa_opt[LD_IPA_SHOW].flag)
    fprintf(sh_cmdfile, "-s ");

#ifdef KEY
  if (IPA_Max_Jobs > 1)
    fprintf(sh_cmdfile, "-j %u ", IPA_Max_Jobs);
#else
  if (IPA_Max_Jobs_Set) 
    fprintf(sh_cmdfile, "-J %u ", IPA_Max_Jobs);
#endif

  fprintf(sh_cmdfile, "\nretval=$?\n");


  // Do cleanup, and return.
  if (!ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag) {
    fprintf(sh_cmdfile, "cleanup; ");
  }
  fprintf(sh_cmdfile, "exit $retval\n");

  fclose(sh_cmdfile);

#ifdef KEY
  // Restore the LD_LIBRARY_PATH that was in effect before the compiler was run.
  if (IPA_old_ld_library_path != NULL) {
    int i;
    int path_len = strlen(IPA_old_ld_library_path);

    // Change ";" back to ":".
    for (i=0; i<path_len; i++)
      if (IPA_old_ld_library_path[i] == ';')
	IPA_old_ld_library_path[i] = ':';

    char *env = (char *) alloca (path_len + strlen("LD_LIBRARY_PATH=") + 5);
    sprintf (env, "LD_LIBRARY_PATH=%s", IPA_old_ld_library_path);
    putenv (env);
  }
#endif

  exec_smake(sh_cmdfile_name);
} // ipacom_doit


// Helper function for get_extra_args.
static void escape_char (char *str)
{
  char *p = str + 1;

  do {
    *str++ = *p++;
  } while (*str != 0);
} /* escape_char */


// Collect any extra arguments that we will tack onto the command line.
// First collect them as a vector of strings, then concatenate them all
// together into a single string.
static const char* get_extra_args(const char* ipaa_filename)
{
  vector<const char*> args;
  args.reserve(16);
  
  switch (ld_ipa_opt[LD_IPA_SHARABLE].flag) {
  case F_MAKE_SHARABLE:
#ifdef KEY
    args.push_back("-TENV:PIC");
#else
    args.push_back("-pic2");
#endif
    break;
  case F_CALL_SHARED:
  case F_CALL_SHARED_RELOC:
#ifndef TARG_MIPS
#if !defined(TARG_SL)
    args.push_back("-pic1");
#endif
#endif
    break;
  case F_NON_SHARED:
    args.push_back("-non_shared");
    break;
  case F_RELOCATABLE:
    if (IPA_Enable_Relocatable_Opt == TRUE)
      args.push_back("-pic1");
    break;
  }
  
  // -IPA:keeplight:=ON, which is the default, means that we keep only
  // the .I files, not the .s files.
  if (ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag && !IPA_Enable_Keeplight)
    args.push_back("-keep");

  if (ld_ipa_opt[LD_IPA_SHOW].flag)
    args.push_back("-show");


  /* If there's an IPAA intermediate file, let WOPT know: */
  if (ipaa_filename) {
    char* buf = (char*) malloc(strlen(ipaa_filename) + 32);
    if (!buf)
      ErrMsg (EC_No_Mem, "extra_args");

    sprintf(buf, "-WOPT:ipaa:ipaa_file=%s", ipaa_filename );
    args.push_back(buf);
  }

  /* If there are -WB,... options, pull them out and add them to the
     * list.  Strip the '-WB,', and treat non-doubled internal commas
     * as delimiters for new arguments (undoubling the doubled ones):
     */
  if (WB_flags) {
    string p = ipc_copy_of (WB_flags);
    while (*p) {
      args.push_back(p);
      while (*p) {
        if (*p == ',') {
          if (p[1] != ',') {
            *p++ = 0;
            break;
          }
          else
            escape_char(p);
        }
        else if (p[0] == '\\' && p[1] != 0)
          escape_char (p);
        p++;
      }
    }
  }

  /* If there are -Yx,... options, pull them out and add them to the
     * list.  Several may be catenated with space delimiters:
     */
  vector<char*> space_ptr; // for restoring spaces overwritten by zero
  if (Y_flags) {
    char* p = Y_flags;
    while (*p) {
      args.push_back(p);
      while (*p) {
        if (*p == ' ') {
          space_ptr.push_back(p);
          *p++ = 0;
          break;
        }
        else if (p[0] == '\\')
          escape_char (p);
        p++;
      }
    }
  }

#ifdef _TARG_MIPS
  /* If there is a -mips[34] option, add it. */
  if (ld_ipa_opt[LD_IPA_ISA].set) {
    switch(ld_ipa_opt[LD_IPA_ISA].flag) {
    case 3:
      args.push_back("-mips3");
      break;
    case 4:
      args.push_back("-mips4");
      break;
    default:
      break;
    }
  }
#endif
#ifdef TARG_LOONGSON
  switch(ld_ipa_opt[LD_IPA_ISA].flag) {
  case TOS_LOONGSON_2e:
    args.push_back("-loongson2e");
    break;
  case TOS_LOONGSON_2f:
    args.push_back("-loongson2f");
    break;
  case TOS_LOONGSON_3:
    args.push_back("-loongson2f");
    break;
  default: // loongson2e
    break;
  }
#endif

  size_t len = 0;
  vector<const char*>::const_iterator i;

  for (i = args.begin(); i != args.end(); ++i)
    len += strlen(*i) + 1;

  char* result = (char*) malloc(len + 1);
  if (!result)
    ErrMsg (EC_No_Mem, "extra_args");    

  result[0] = '\0';

  for (i = args.begin(); i != args.end(); ++i) {
    strcat(result, *i);
    strcat(result, " ");
  }

  // now restore spaces in Y_flags that were overwritten by zeros
  for (size_t idx = 0; idx < space_ptr.size(); idx++) {
    Is_True(*space_ptr[idx] == 0, ("space_ptr must point to 0"));
    *space_ptr[idx] = ' ';
  }

  return result;
} /* get_extra_args */

static const char* get_extra_symtab_args(const ARGV& argv)
{
  const char* result = get_extra_args(0);

  for (ARGV::const_iterator i = argv.begin(); i != argv.end(); ++i) {
    const char* const debug_flag = "-DEBUG";
    const char* const G_flag = "-G";
    const char* const TARG_flag = "-TARG";
    const char* const OPT_flag = "-OPT";
    const int debug_len = 6;
    const int G_len = 2;
    const int TARG_len = 5;
    const int OPT_len = 4;
    bool flag_found = false;

    // The link line contains -r.  That means we don't have enough information
    // from the link line alone to determine whether the symtab should be
    // compiled shared or nonshared.  We have to look at how one of the other
    // files was compiled.
    if (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_RELOCATABLE &&
                IPA_Enable_Relocatable_Opt != TRUE) {
      const char* const non_shared_flag = "-non_shared";
      if (strcmp(*i, non_shared_flag) == 0)
        flag_found = true;
    }

    if ((strncmp(*i, debug_flag, debug_len) == 0) ||
            (strncmp(*i, G_flag, G_len) == 0) ||
            (strncmp(*i, OPT_flag, OPT_len) == 0) ||
            (strncmp(*i, TARG_flag, TARG_len) == 0))

      flag_found = true;

    if (flag_found == true) {
      char* buf = static_cast<char*>(malloc(strlen(result) +
                                            strlen(*i) + 2));
      if (!buf)
        ErrMsg (EC_No_Mem, "extra_symtab_args");
      strcpy(buf, result);
      strcat(buf, " ");
      strcat(buf, *i);
      free(const_cast<char*>(result));
      result = buf;
     }
  }

  return result;
}

static void exec_smake (char* sh_cmdfile_name)
{
  /* Clear the trace file: */
  Set_Trace_File ( NULL );

#ifdef _OPENMP
  /* The exec process will cause mp slaves, if any, to
       be killed.  They will in turn send a signal to the
       master process, which is the new process.  This process
       in turn has a signal handler that dies when it finds out
       that its slaves die.  This way, if we ever compile -mp,
       we kill the child processes before doing the exec */
  mp_destroy_();
#endif

  // Call the shell.
  const char* sh_name = "/bin/sh";

  const int argc = 2;
  char* argv[argc+1];
  argv[0] = const_cast<char*>(sh_name);
  argv[1] = sh_cmdfile_name;
  argv[2] = 0;
  
  execve (sh_name, argv, environ_vars);

  // if the first try fails, use the user's search path
  execvp ("sh", argv);

  Fail_FmtAssertion ("ipa: exec sh failed");
}

#ifdef KEY
extern "C" {
// Allow ipa_link to call the error routines.

void
Ipalink_Set_Error_Phase (char *name)
{
  Set_Error_Phase (name);
}

void
Ipalink_ErrMsg_EC_infile (char *name)
{ 
  ErrMsg(EC_Ipa_Infile, name);
}

void
Ipalink_ErrMsg_EC_outfile (char *name)
{ 
  ErrMsg(EC_Ipa_Outfile, name);
}

}	// extern "C"
#endif

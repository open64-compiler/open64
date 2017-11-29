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
#include <sys/types.h>
#ifdef __MINGW32__
#include <WINDOWS.h>
#else
#include <sys/mman.h>
#endif
#include <sys/stat.h>
#include <unistd.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <fcntl.h> 
#include <errno.h>
#include <signal.h>
#include <bstring.h>
#define USE_STANDARD_TYPES
#include "defs.h"
#include "cxx_memory.h"
#include "ipa_section.h"
#include "wn.h"
#include "erglob.h"
#include "errors.h"
#include "wn_simp.h"
#include "ir_reader.h"
#include "strtab.h"                 
#include "stab.h"                  
#include "irbdata.h"              
#include "dwarf_DST_mem.h"
#include "elf_stuff.h"
#include "pu_info.h"
#include "config_targ.h" 
#include "config_elf_targ.h"
#include "ipa_lno_file.h"
#include "config_debug.h"
#include "ir_bcom.h"
#include "ipa_lno_info.h"
#include "ipl_summary.h"

// Put this here for now.  Later move it on to /usr/include/sys/elf.h and 
// change SHT_MIPS_NUM to 42.  
#define SHT_MIPS_IPALNO  (SHT_LOPROC + 42)

#if defined(linux) || defined(BUILD_OS_DARWIN)
#define MAPPED_SIZE 0x400000
#endif

// Put this here for now.  Later move it on to /usr/sys/include/elfwhirl.h? 
static char IPALNO_REVISION[] = "IPALNO:1.1"; 

static void (*old_sigsegv)(int);   /* the previous signal handler */
static void (*old_sigbus)(int);   /* the previous signal handler */

//-----------------------------------------------------------------------
// NAME: Ir_Lno_Signal_Handler
// FUNCTION: This signal handler replaces the old signal handlers for 
//   SIGSEGV and SIGBUS while using memory mapped I/O to write out the 
//   IPA.LNO file.  Here 'sig' is the type of signal, and 'err_num' is
//   the number of the error which caused the signal.  
// NOTES: This routine is patterned after the routine ir_bwrite_signal_ 
//   handler() in common/com/ir_bwrite.cxx.  It is toggled during calls 
//   to ir_b_save_buf() in common/com/ir_bcom.cxx. 
//-----------------------------------------------------------------------

static void Ir_Lno_Signal_Handler(int sig, 
				  int err_num)
{
  void (*old_handler)(int) = 0;

  // Doing memory mapped I/O: This is not a normal SIGSEGV or SIGBUS 
  // Invoke a fatal error instead of normal handling.  The variable 
  // 'Doing_mmapped_io' is defined in common/com/ir_bcom.cxx

  errno = 0;
  char *err_str = strerror(err_num);
  
  if (Doing_mmapped_io && errno == 0)
    Fatal_Error("I/O error in %s: %s", Current_Output ?
      Current_Output->file_name : "mmapped object", err_str);

  // Otherwise, handle this as a normal SIGSEGV or SIGBUS
  switch (sig) {
#ifndef __MINGW32__
  case SIGBUS:
    old_handler = old_sigbus;
    break;
#endif /* __MINGW32__ */
  case SIGSEGV:
    old_handler = old_sigsegv;
    break;
  }
  if (old_handler == SIG_DFL) {
    // Resignal - will get default handler
#ifdef __MINGW32__
    raise(sig);
#else
    kill(getpid(), sig);
#endif /* __MINGW32__ */
  } else if (old_handler != SIG_IGN) {
    // Call old handler  
    (*old_handler)(sig);
  }
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Create_Or_Get_Section
// FUNCTION: Create a section corresponding to the 'sh_info' (section 
//   number) and 'name' (section name) for the IPALNO file.
//-----------------------------------------------------------------------

Section* IPA_LNO_WRITE_FILE::Create_Or_Get_Section(Elf64_Word sh_info,
                                                   const char *name)
{
  register INT i;
  Elf64_Word type = SHT_MIPS_IPALNO;
  const INT num_sections = IPA_LNO_SECTION_COUNT; 
  Is_True(ofl, ("Create_Section: Missing Output File"));  

  for (i = 0; i < ofl->num_of_section; i++) {
    if ((ofl->section_list[i].shdr.sh_info == sh_info) &&
      (strcmp (ofl->section_list[i].name, name) == 0)) {
      ofl->cur_section = ofl->section_list + i;
      return ofl->cur_section;
    }
  }

  if (ofl->num_of_section == 0) {
    ofl->max_num_of_section = num_sections;
    ofl->section_list =
	(Section *)malloc(ofl->max_num_of_section * sizeof(Section));
    FmtAssert (ofl->section_list, ("No more memory"));
  } else if (ofl->max_num_of_section == ofl->num_of_section) {
    ofl->max_num_of_section *= 2;
    ofl->section_list = (Section *) realloc
	(ofl->section_list,
	 ofl->max_num_of_section * sizeof(Section));
    FmtAssert (ofl->section_list, ("No more memory"));
  }

  ofl->cur_section = ofl->section_list + ofl->num_of_section;
  ofl->num_of_section += 1;
  memset (ofl->cur_section, 0, sizeof(Section));
  ofl->cur_section->name = name;
  ofl->cur_section->shdr.sh_info = sh_info;
  ofl->cur_section->shdr.sh_type = type;
  return ofl->cur_section;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Create_Sections
// FUNCTION: Create the section headers for the file.  They are 
//   created in the order specified by the calls to Get_Write_Section(). 
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Create_Sections()
{
 Is_True(ofl, ("Create_Sections: Missing Output File"));  
 Create_Or_Get_Section(IPA_PROCEDURE, IPA_PROCEDURE_NAME);
 Create_Or_Get_Section(IPA_PROJECTED_REGION, IPA_PROJECTED_REGION_NAME);
 Create_Or_Get_Section(IPA_PROJECTED_ARRAY, IPA_PROJECTED_ARRAY_NAME);
 Create_Or_Get_Section(IPA_TERM_ARRAY, IPA_TERM_ARRAY_NAME);
 Create_Or_Get_Section(IPA_IVAR, IPA_IVAR_NAME);
 Create_Or_Get_Section(IPA_FORMAL, IPA_FORMAL_NAME);
 Create_Or_Get_Section(IPA_GLOBAL, IPA_GLOBAL_NAME);
 Create_Or_Get_Section(IPA_STRINGS, IPA_STRINGS_NAME);
 Create_Or_Get_Section(IPA_REVISION, IPA_REVISION_NAME);
}

#define DEFAULT_TMPDIR          "/usr/tmp"
#define DEFAULT_TEMPLATE        "/elf_wnXXXXXX"

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Create_Temp_File
// FUNCTION: Create a temporary file in the directory "/usr/tmp" as speci-
//   fied by the template "/elf_wnXXXXXX". 
//-----------------------------------------------------------------------

INT IPA_LNO_WRITE_FILE::Create_Temp_File()
{
  register const char *tmpdir;
  register char *path;
  register int fd;

  if ((tmpdir = getenv("TMPDIR")) == 0)
    tmpdir = DEFAULT_TMPDIR;
  path = (char *) malloc(strlen(tmpdir) + strlen(DEFAULT_TEMPLATE) + 1);
  if (path == 0)
    return -1;
#ifdef __MINGW32__
  {
    int mode = O_RDWR | O_CREAT | O_EXCL ;
    do {
      strcpy (path, tmpdir);
      strcat (path, DEFAULT_TEMPLATE);
      mktemp( path );
    } while( (fd = open(path, mode)) < 0 );
  }
#else
  strcpy(path, tmpdir);
  strcat(path, DEFAULT_TEMPLATE);
  fd = mkstemp (path);
#endif /* __MINGW32__ */
  if (fd != -1)
    unlink (path);
  ofl->file_name = path;
  return fd;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Open_Write_File
// FUNCTION: Create and open the file.  If 'file_name' is null, create a 
//   tempoarry file, otherwise create a file with name 'file_name', if 
//   one does not already exist, or open the file with name 'file_name' 
//   and truncate it to zero length.  Returns with 'ofl == NULL' if an
//   error occurs: 
//     (1) Could not malloc 'ofl' structure (not enough memory). 
//     (2) Could not open file. 
//     (3) Could not memory map file. 
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Open_Write_File(char *file_name)
{
#ifndef __MINGW32__
  // Replace the existing signal handlers for SIGSEV and SIGBUS with 
  // Ir_Lno_Signal_Handler()

  if (old_sigsegv == 0)
    old_sigsegv = signal(SIGSEGV, reinterpret_cast<void (*)(int)>
      (Ir_Lno_Signal_Handler));

  if (old_sigbus == 0)
    old_sigbus = signal (SIGBUS, reinterpret_cast<void (*)(int)>
      (Ir_Lno_Signal_Handler)); 
#endif /* __MINGW32__ */

  // Create a file descriptor for the output file 
  ofl = (Output_File *) malloc(sizeof(Output_File));
  if (!ofl) 
    return;

  // Create and open the file.  The file is truncated to zero length 
  // if it already exists, otherwise it is created and initially zero
  // length. 

  if (file_name == 0) {
    ofl->output_fd = Create_Temp_File();
  } else {
    ofl->file_name = file_name;
    ofl->output_fd = open((const char *)(file_name), 
      (int) (O_RDWR|O_CREAT|O_TRUNC), (mode_t) 0644);
  }
  if (ofl->output_fd < 0) {
    ofl = NULL; 
    return;
  } 

#if defined(linux) || defined(BUILD_OS_DARWIN)
    ftruncate(ofl->output_fd, MAPPED_SIZE);
#endif

  ofl->section_list = NULL;
  ofl->cur_section = NULL;
  ofl->num_of_section = 0;
  
  // Initialize the output map
  if (ir_b_create_map(ofl) == (char *) (-1)) {
    ofl = NULL; 
    return;
  } 

  // Leave space for the Elf header
#ifdef __ALWAYS_USE_64BIT_ELF__
    ofl->file_size = sizeof(Elf64_Ehdr);
#else
    ofl->file_size = Use_32_Bit_Pointers ?
	sizeof(Elf32_Ehdr) : sizeof(Elf64_Ehdr);
#endif

  // Create all the sections 
  Create_Sections();
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Write_Section
// FUNCTION: Write out the section number 'sh_info' which has the given 
//   'name'.  Actual text for the section is given in the buffer 'buf' 
//   which has length 'size'. 
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Write_Section(Elf64_Word sh_info,
                                       const char* name,
				       void* buf, 
				       INT size)
{
  Is_True(ofl, ("Write_Section: Missing Output File"));  
  Section *s;
  s = Create_Or_Get_Section(sh_info, name);
  ofl->file_size = ir_b_align(ofl->file_size, sizeof(mINT64), 0);
  s->shdr.sh_offset = ofl->file_size;
  INT offset = (INT) ir_b_save_buf(buf, size, sizeof(mINT64), 0, ofl);
  s->shdr.sh_size = ofl->file_size - s->shdr.sh_offset;
  s->shdr.sh_addralign = sizeof(mINT64);
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Write_Headers
// FUNCTION: Write out the elf header and the section headers for the 
//   file.  There are actually ofl->num_of_section + 2 sections: a NULL
//   section, the normal sections, and a section for the string table. 
//   Here 'e_shoff' is the offset in the file to the place where the 
//   section headers are to be written (which is after the elf header,
//   all of the normal sections, and the string table), and 'strtab_sec' 
//   is a pointer to the string table section header.
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Write_Headers(Elf64_Off e_shoff, 
				    Elf64_Shdr* strtab_sec)
{
  INT i;
  char *str;
  Elf64_Shdr *shdr;
  Elf64_Ehdr *ehdr;
  char *base_addr = ofl->map_addr;
  Is_True(ofl, ("Write_Headers: Missing Output File"));  

  // Grow the file to include space for the headers
  // There are 2 extra sections: the null and the section string table
  ofl->file_size += sizeof(Elf64_Shdr) * (ofl->num_of_section + 2);

  // Make sure the entire file is mapped
  if (ofl->file_size >= ofl->mapped_size)
    ir_b_grow_map(0, ofl);

  //  Write the Elf header
  ehdr = (Elf64_Ehdr *) ofl->map_addr;
  strcpy((char *) ehdr->e_ident, ELFMAG);
  ehdr->e_ident[EI_CLASS] = ELFCLASS64;
  ehdr->e_ident[EI_DATA] = ELFDATA2MSB; /* assume MSB for now */
  ehdr->e_ident[EI_VERSION] = EV_CURRENT;
  ehdr->e_type = ET_IR;
  ehdr->e_machine = Get_Elf_Target_Machine();
  ehdr->e_version = EV_CURRENT;
  ehdr->e_shoff = e_shoff;
  ehdr->e_flags = Config_ELF_From_Target (
	! Use_32_Bit_Pointers, FALSE, Target_ISA);
  ehdr->e_ehsize = sizeof(Elf64_Ehdr);
  ehdr->e_shentsize = sizeof(Elf64_Shdr);
  ehdr->e_shnum = ofl->num_of_section + 2;
  ehdr->e_shstrndx = ofl->num_of_section + 1;

  // Write the section string table
  // This consists of null terminated names of each of the sections. 
  // Recall that the first is the NULL section, followed by the normal 
  // sections, followed by the name of the string table. 

  // Name of the null section
  str = base_addr + strtab_sec->sh_offset;
  str[0] = '\0';
  str++;
  // Names of each of the normal sections 
  for (i = 0; i < ofl->num_of_section; i++) {
      strcpy(str, ofl->section_list[i].name);
      str += strlen(str) + 1;
  }
  // Name of the section header string table 
  strcpy(str, ELF_SHSTRTAB);

  // Simple check to see if layout has been done right 
  // If the string table is written past the place we think the   
  //   section headers should go, we have an error. 
  if ((INTPTR) str + strlen(ELF_SHSTRTAB) + 1 >
    e_shoff + (INTPTR) base_addr)
    ErrMsg (EC_IR_Scn_Write, "Section Header String Table", ofl->file_name);

  // Last, write the section header table
  shdr = (Elf64_Shdr *) (base_addr + e_shoff);
  // ... First, the null section header
  memset(shdr, 0, sizeof(Elf64_Shdr)); 
  shdr++;
  // ... Then, each of the normal section headers
  for (i = 0; i < ofl->num_of_section; i++, shdr++)
      memcpy(shdr, &(ofl->section_list[i].shdr), sizeof(Elf64_Shdr));
  // ... Finally, the section string table section header
  memcpy(shdr, strtab_sec, sizeof(Elf64_Shdr));
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Create_String_Table_Section
// FUNCTION: Layout the string table, and grow the file to accomodate 
//   the section headers.
//-----------------------------------------------------------------------

Elf64_Off IPA_LNO_WRITE_FILE::Create_String_Table_Section(
  Elf64_Shdr *strtab_sec)
{
  INT i;
  Elf64_Off e_shoff;
  Elf64_Word strtab_size = 1;

  // Find out how large the string table needs to be to accomodate the
  // the names of all of the strings.
  for (i = 0; i < ofl->num_of_section; i++) {
    register Section *sec = &(ofl->section_list[i]);
    sec->shdr.sh_name = strtab_size;
    strtab_size += strlen(sec->name) + 1;
  }

  // Fill in the string table header information
  memset(strtab_sec, 0, sizeof(Elf64_Shdr));
  strtab_sec->sh_name = strtab_size;
  strtab_size += strlen (ELF_SHSTRTAB) + 1;
  strtab_sec->sh_type = SHT_STRTAB;
  strtab_sec->sh_size = strtab_size;
  strtab_sec->sh_offset = ofl->file_size;
  ofl->file_size += strtab_size;
  strtab_sec->sh_addralign = 1;
  strtab_sec->sh_entsize = 1;

  // Align the file
  ofl->file_size = ir_b_align(ofl->file_size,
#if defined(__GNUC__)
            __alignof__(Elf64_Shdr),
#else			      
      __builtin_alignof(Elf64_Shdr),
#endif
      0);
  e_shoff = ofl->file_size;
  return e_shoff;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Write_Revision
// FUNCTION: Write the revision number into the IPALNO file. 
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Write_Revision()
{
  Section *s;
  s = Create_Or_Get_Section(IPA_REVISION, IPA_REVISION_NAME);
  s->shdr.sh_offset = ofl->file_size;
  INT offset = (INT) ir_b_save_buf(IPALNO_REVISION, strlen(IPALNO_REVISION) 
    + 1, 1, 0, ofl);  
  s->shdr.sh_size = ofl->file_size - s->shdr.sh_offset;
  s->shdr.sh_addralign = 1; 
  s->shdr.sh_type = SHT_MIPS_IPALNO;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Write_Cleanup
// FUNCTION: Reset the values in the Output_File structure and unmap the 
//   output file's memory map. 
//-----------------------------------------------------------------------

void IPA_LNO_WRITE_FILE::Write_Cleanup()
{
  FmtAssert(ofl != NULL, ("Write_Cleanup: Missing Output_File")); 
  ofl->output_fd = -1;
  if (ofl->num_of_section > 0)
    free(ofl->section_list);
  ofl->num_of_section = 0;
  ofl->section_list = NULL;
#ifndef __MINGW32__ 
  munmap((void *) ofl->map_addr, (size_t) ofl->mapped_size);
#endif /* __MINGW32__ */
  ofl->map_addr = NULL;
  ofl->file_size = 0;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_WRITE_FILE::Close_Write_File
// FUNCTION: Given that all of the sections have been written and all of 
//   the information has been gathered for the section headers, create 
//   the section string table, write the section headers, and unmap 
//   the output file memory map. 
//-----------------------------------------------------------------------

INT IPA_LNO_WRITE_FILE::Close_Write_File()
{
  FmtAssert(ofl != NULL, ("Close_Write_File: Missing Output_File")); 
  Write_Revision(); 
  Elf64_Shdr strtab_sec;
  Elf64_Off e_shoff = Create_String_Table_Section(&strtab_sec);
  Write_Headers(e_shoff, &strtab_sec);
  if (ftruncate(ofl->output_fd, ofl->file_size) != 0)
    return IPALNO_CLOSE_ERROR; 
  close(ofl->output_fd);
  Write_Cleanup(); 
  return IPALNO_SUCCESS;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Check_Section_Headers
// FUNCTION: Check the section headers for the memory mapped file.  
//   If they are OK, return IPALNO_SUCCESS, otherwise, return 
//     IPALNO_FORMAT_ERROR: if there is a format mismatch. 
//     IPALNO_REVISION_MISMATCH: if there is a revision mismatch. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Check_Section_Headers() 
{
  register INT i;
  Elf64_Ehdr *ehdr;
  Elf64_Shdr *shdr;
  char *shstrtab;

  FmtAssert(ifl != NULL, ("Check_Sections_Headers: Missing Input_File")); 
  char* baseaddr = (char *) ifl->mapped_address; 
  ehdr = (Elf64_Ehdr *) baseaddr;
  shdr = (Elf64_Shdr *) ((char *) baseaddr + ehdr->e_shoff);

  errno = ENOEXEC;

  if (shdr[ehdr->e_shstrndx].sh_offset >= ifl->mapped_size ||
      shdr[ehdr->e_shstrndx].sh_offset +
      shdr[ehdr->e_shstrndx].sh_size > ifl->mapped_size)
    return IPALNO_FORMAT_ERROR;

  shstrtab = baseaddr + shdr[ehdr->e_shstrndx].sh_offset;

  for (i = 1; i < ehdr->e_shnum; i++) {
    register Elf64_Shdr *sec = &(shdr[i]);

    if (sec->sh_offset >= ifl->mapped_size 
        || sec->sh_offset + sec->sh_size > ifl->mapped_size)
      return IPALNO_FORMAT_ERROR;
    if (sec->sh_name >= shdr[ehdr->e_shstrndx].sh_size)
      return IPALNO_FORMAT_ERROR;
    if (sec->sh_link >= ehdr->e_shnum)
      return IPALNO_FORMAT_ERROR;
    if (sec->sh_addralign & (sec->sh_addralign - 1))
      return IPALNO_FORMAT_ERROR;
    if (sec->sh_addralign >1 &&
      (long)(baseaddr + sec->sh_offset) & ((sec->sh_addralign - 1)))
      return IPALNO_FORMAT_ERROR;

    // Search for and verify the revision string 
    if (sec->sh_type == SHT_MIPS_IPALNO &&
	strcmp(shstrtab + sec->sh_name, IPA_REVISION_NAME) == 0) {

      register char *p = baseaddr + sec->sh_offset;
      register char *eob = p + sec->sh_size - 1;

      register int match = 0;

      if (*eob != 0)
        // Section not NULL-terminated */
        return IPALNO_FORMAT_ERROR;

      while (p <= eob) {
	if (strncmp ("IPALNO:", p, 7) == 0) {
	  strcpy(ifl->file_revision, p);
	  if (strcmp(IPALNO_REVISION, p) == 0) {
	    match = 1;
	    break;
	  }
	}
	p += strlen (p) + 1;
      }

      if (!match && DEBUG_IPALNO_Version_Check)
	return IPALNO_REVISION_MISMATCH;
    }
  }

  errno = 0;
  return IPALNO_SUCCESS;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Check_Elf_Header
// FUNCTION: Verify that the elf header of the input file is correct. 
//   If it is, return IPALNO_SUCCESS, otherwise return 
//     IPALNO_FORMAT_ERROR: If there is a format error. 
//     IPALNO_ABI_MISMATCH: If the expected ABI is not found.  
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Check_Elf_Header()
{
  Elf64_Shdr *shdr;
  Elf64_Ehdr *ehdr;
  BOOL is_64bit;
  INT isa;

  FmtAssert(ifl != NULL, ("Check_Elf_Header: Missing Input_File")); 
  if (ifl->mapped_size < sizeof(Elf64_Ehdr))
    return IPALNO_FORMAT_ERROR;

  char* baseaddr = (char *) ifl->mapped_address; 
  ehdr = (Elf64_Ehdr *) baseaddr;

  if (IS_ELF (*ehdr)) {
    if (ehdr->e_ident[EI_CLASS] != ELFCLASS64)
      return IPALNO_FORMAT_ERROR;
    if (ehdr->e_ident[EI_VERSION] != EV_CURRENT ||
      ehdr->e_version != EV_CURRENT)
      return IPALNO_FORMAT_ERROR;
    if (ehdr->e_type != ET_IR ||
      ehdr->e_machine != Get_Elf_Target_Machine() ||
      ehdr->e_shentsize != sizeof(Elf64_Shdr))
      return IPALNO_FORMAT_ERROR;
    Config_Target_From_ELF (ehdr->e_flags, &is_64bit, &isa);
    if ( ! Set_Target_ABI (is_64bit, isa))
      return IPALNO_ABI_MISMATCH;
    if (ehdr->e_shstrndx >= ehdr->e_shnum)
      return IPALNO_FORMAT_ERROR;
    if (ehdr->e_shoff >= ifl->mapped_size ||
        ehdr->e_shoff + ehdr->e_shnum * sizeof(Elf64_Shdr) > ifl->mapped_size)
      return IPALNO_FORMAT_ERROR;
    shdr = (Elf64_Shdr *) (baseaddr + ehdr->e_shoff);
#if defined(__GNUC__)
    if ((long) shdr & (__alignof__(Elf64_Shdr) - 1))
#else
    if ((long) shdr & (__builtin_alignof(Elf64_Shdr) - 1))
#endif
      return IPALNO_FORMAT_ERROR;
    return IPALNO_SUCCESS;
  }

  return IPALNO_FORMAT_ERROR;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Check_Input
// FUNCTION: Check the input file format. Return IPALNO_SUCCESS if there 
//   are no problems.  Otherwise, return: 
//   IPALNO_FORMAT_ERROR: if there is a formatting error
//   IPALNO_ABI_MISMATCH: if there is an ABI mismatch in the elf header
//   IPALNO_REVISION_MISMATCH: if there is a revision number mismatch 
//     in the section header information. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Check_Input()
{
  INT st;
  FmtAssert(ifl != NULL, ("Check_Input: Missing Input_File")); 
  if ((st = Check_Elf_Header()) < 0)
    return st;
  if ((st = Check_Section_Headers()) <= 0)
    return st;
  return IPALNO_SUCCESS;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Open_Read_File
// FUNCTION: Read the IPALNO file with name 'input_file'.  Return 
//   IPALNO_READER_ERROR: if there is an error opening, fstating, or 
//     mmapping the file. 
//   IPALNO_FORMAT_ERROR: if there is a formatting error
//   IPALNO_ABI_MISMATCH: if there is an ABI mismatch in the elf header
//   IPALNO_REVISION_MISMATCH: if there is a revision number mismatch 
//     in the section header information. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Open_Read_File(const char input_file[]) 
{
  int fd;
  INT st;
  struct stat stat_buf;
  register char *map_addr;

  if (input_file == NULL) {
    errno = ENOENT; 
    return IPALNO_READER_ERROR; 
  } 

  fd = open(input_file, O_RDONLY);
  if (fd < 0)
    return IPALNO_READER_ERROR;

  if (fstat(fd, &stat_buf) != 0)
    return IPALNO_READER_ERROR;

#ifdef __MINGW32__
  map_addr = NULL;
  HANDLE map_addr_handle = NULL;
  map_addr_handle =
    CreateFileMapping((HANDLE) _get_osfhandle(fd), NULL,
		      PAGE_READWRITE, 0, stat_buf.st_size, input_file);
  if (map_addr_handle)
    map_addr = (char *)MapViewOfFileEx(map_addr_handle,
				       FILE_MAP_COPY,
				       0, 0, stat_buf.st_size, 0);

  if (map_addr == NULL) {
    close(fd);
    return IPALNO_READER_ERROR;
  }

#else
  map_addr = (char *) mmap(0, stat_buf.st_size, PROT_READ|PROT_WRITE,
    MAP_PRIVATE, fd, 0);
  if (map_addr == MAP_FAILED) {
    close (fd);
    return IPALNO_READER_ERROR;
  }
#endif /* __MING32 __ */

  // If everything is fine, store information about the file 
  ifl = (Input_File *) malloc(sizeof(Input_File));
  ifl->mapped_address = map_addr; 
  ifl->mapped_size = stat_buf.st_size;

  close(fd);
  if ((st = Check_Input()) < 0) {
#ifndef __MINGW32__
    munmap(map_addr, stat_buf.st_size);
#endif /* __MINGW__ */
    return st;
  }
  return IPALNO_SUCCESS; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Section_Header
// FUNCTION: Return pointer to section header number 'info' in the input 
//   file. 
//-----------------------------------------------------------------------

Elf64_Shdr* IPA_LNO_READ_FILE::Section_Header(Elf64_Word info)
{
  register INT i;
  FmtAssert(ifl != NULL, ("Section_Header: Missing Input_File")); 
  void* mapped_address = ifl->mapped_address; 
  Elf64_Ehdr *eh = (Elf64_Ehdr *) mapped_address;
  Elf64_Shdr *sh;

  // Change the type later. 
  Elf64_Word type = SHT_MIPS_IPALNO;  
  if (mapped_address == 0) {
    errno = EINVAL;
    return NULL;
  }
  errno = 0;
  sh = (Elf64_Shdr *) ((char *) mapped_address + eh->e_shoff);
  sh += 1;
  for (i = 1; i < eh->e_shnum; i++) {
    if ((sh->sh_type == type) && (sh->sh_info == info))
      return sh;
    sh += 1;
  }
  return NULL;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Section_Address
// FUNCTION: Return pointer to section header number 'info' in the input 
//   file. 
//-----------------------------------------------------------------------

void* IPA_LNO_READ_FILE::Section_Address(Elf64_Word info)
{
  FmtAssert(ifl != NULL, ("Section_Address: Missing Input_File")); 
  Elf64_Shdr* shdr = Section_Header(info); 
  if (shdr == NULL)
    return NULL; 
  return (void*) ((char*) ifl->mapped_address + shdr->sh_offset); 
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Section_Size
// FUNCTION: Return pointer to section header number 'info' in the input 
//   file. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Section_Size(Elf64_Word info)
{ 
  FmtAssert(ifl != NULL, ("Section_Size: Missing Input_File")); 
  Elf64_Shdr* shdr = Section_Header(info); 
  if (shdr == NULL)
    return 0;
  return (INT) shdr->sh_size;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Close_Read_File 
// FUNCTION: Unmemmap the input file, if there is one.  
//-----------------------------------------------------------------------

void IPA_LNO_READ_FILE::Close_Read_File()
{ 
#ifndef __MINGW32__
  if (ifl != NULL)  
    munmap(ifl->mapped_address, ifl->mapped_size); 
#endif /* __MINGW32__ */
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Procedure
// FUNCTION: Return a IPA_LNO_SUMMARY_PROCEDURE* to the 'i'th procedure 
//   on the IPA_LNO_READ_FILE. 
//-----------------------------------------------------------------------

IPA_LNO_SUMMARY_PROCEDURE* IPA_LNO_READ_FILE::Procedure(INT i)
{
  IPA_LNO_SUMMARY_PROCEDURE* sa 
    = (IPA_LNO_SUMMARY_PROCEDURE*) Section_Address(IPA_PROCEDURE); 
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Procedure_Count
// FUNCTION: Return the number of IPA_LNO_SUMMARY_PROCEDUREs on the 
//   IPA_LNO_READ_FILE. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Procedure_Count()
{
  return Section_Size(IPA_PROCEDURE) / sizeof(IPA_LNO_SUMMARY_PROCEDURE); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Projected_Region
// FUNCTION: Return a PROJECTED_REGION* to the 'i'th projected region
//   on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

PROJECTED_REGION* IPA_LNO_READ_FILE::Projected_Region(INT i)
{
  PROJECTED_REGION* sa 
    = (PROJECTED_REGION*) Section_Address(IPA_PROJECTED_REGION);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Projected_Region_Count
// FUNCTION: Return the number of PROJECTED_REGIONs on the 
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Projected_Region_Count()
{
  return Section_Size(IPA_PROJECTED_REGION) / sizeof(PROJECTED_REGION); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Projected_Node
// FUNCTION: Return a PROJECTED_NODE* to the 'i'th projected node 
//   on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

PROJECTED_NODE* IPA_LNO_READ_FILE::Projected_Node(INT i)
{
  PROJECTED_NODE* sa = (PROJECTED_NODE*) Section_Address(IPA_PROJECTED_ARRAY); 
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Projected_Node_Count
// FUNCTION: Return the number of PROJECTED_NODEs on the 
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Projected_Node_Count()
{
  return Section_Size(IPA_PROJECTED_ARRAY) / sizeof(PROJECTED_NODE); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Term
// FUNCTION: Return a TERM* to the 'i'th term on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

TERM* IPA_LNO_READ_FILE::Term(INT i)
{
  TERM* sa = (TERM*) Section_Address(IPA_TERM_ARRAY);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Term_Count
// FUNCTION: Return the number of TERMs on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Term_Count()
{
  return Section_Size(IPA_TERM_ARRAY) / sizeof(TERM); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Ivar
// FUNCTION: Return an IVAR* to the 'i'th IVAR on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

IVAR* IPA_LNO_READ_FILE::Ivar(INT i)
{
  if (i < Original_Ivar_Count()) { 
    IVAR* sa = (IVAR*) Section_Address(IPA_IVAR);
    return &sa[i]; 
  } else { 
    return &_extra_ivars[i - Original_Ivar_Count()];
  }  
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Add_Translated_Ivar
// FUNCTION: Add a new IVAR for a translated symbol.  Return the index of 
//   the new IVAR. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Add_Translated_Ivar(const IVAR& ivar)
{
  _extra_ivars.AddElement(ivar);
  return Original_Ivar_Count() + _extra_ivars.Lastidx(); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Add_Translated_Ivar_Unique
// FUNCTION: Add a new IVAR for a translated symbol, if an IVAR for that
//   symbol not already exist.  In either case, return the index of the 
//   symbol.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Add_Translated_Ivar_Unique(const IVAR& ivar)
{
  for (INT i = 0; i < Ivar_Count(); i++) {
    if (*Ivar(i) == ivar)
      return i; 
  } 
  return Add_Translated_Ivar(ivar); 
} 
      

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Original_Ivar_Count
// FUNCTION: Return the original number of IVARs associated with the 
//   IPA_LNO_READ_FILE. 
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Original_Ivar_Count()
{
  return Section_Size(IPA_IVAR) / sizeof(IVAR); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Ivar_Count
// FUNCTION: Return the total number IVARs associated with the 
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Ivar_Count()
{
  INT old_ivar_count = Original_Ivar_Count();
  INT new_ivar_count = _extra_ivars.Lastidx() + 1;
  return old_ivar_count + new_ivar_count;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Formal
// FUNCTION: Return a IPA_LNO_SUMMARY_FORMAL* to the 'i'th formal on the
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

IPA_LNO_SUMMARY_FORMAL* IPA_LNO_READ_FILE::Formal(INT i)
{
  IPA_LNO_SUMMARY_FORMAL* sa 
    = (IPA_LNO_SUMMARY_FORMAL*) Section_Address(IPA_FORMAL);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Formal_Count
// FUNCTION: Return the number of IPA_LNO_SUMMARY_FORMALs on the 
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Formal_Count()
{
  return Section_Size(IPA_FORMAL) / sizeof(IPA_LNO_SUMMARY_FORMAL); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Global
// FUNCTION: Return a IPA_LNO_SUMMARY_GLOBAL* to the 'i'th global on the 
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

IPA_LNO_SUMMARY_GLOBAL* IPA_LNO_READ_FILE::Global(INT i)
{
  IPA_LNO_SUMMARY_GLOBAL* sa 
    = (IPA_LNO_SUMMARY_GLOBAL*) Section_Address(IPA_GLOBAL);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Global_Count
// FUNCTION: Return the number of IPA_LNO_SUMMARY_GLOBALs on the
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Global_Count()
{
  return Section_Size(IPA_GLOBAL) / sizeof(IPA_LNO_SUMMARY_GLOBAL); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Value
// FUNCTION: Return a SUMMARY_VALUE* to the 'i'th SUMMARY_VALUE on the
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

SUMMARY_VALUE* IPA_LNO_READ_FILE::Value(INT i)
{
  SUMMARY_VALUE* sa = (SUMMARY_VALUE*) Section_Address(IPA_VALUE);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Value_Count
// FUNCTION: Return the number of SUMMARY_VALUEs on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Value_Count()
{
  return Section_Size(IPA_VALUE) / sizeof(SUMMARY_VALUE); 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Expr
// FUNCTION: Return a SUMMARY_EXPR* to the 'i'th SUMMARY_EXPR on the
//   IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

SUMMARY_EXPR* IPA_LNO_READ_FILE::Expr(INT i)
{
  SUMMARY_EXPR* sa = (SUMMARY_EXPR*) Section_Address(IPA_EXPR);
  return &sa[i]; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_READ_FILE::Expr_Count
// FUNCTION: Return the number of SUMMARY_EXPRs on the IPA_LNO_READ_FILE.
//-----------------------------------------------------------------------

INT IPA_LNO_READ_FILE::Expr_Count()
{
  return Section_Size(IPA_EXPR) / sizeof(SUMMARY_EXPR); 
} 


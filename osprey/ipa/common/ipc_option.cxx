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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipc_option.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/common/ipc_option.cxx,v $
 *
 * Description: Option processing for -INLINE options.
 * 
 * =====================================================================
 * =====================================================================
 */

#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#ifndef __MINGW32__
#include <sys/mman.h>
#endif
#include <ctype.h>
#include <string.h>
#include <ar.h>         /* for support of -INLINE:library= */
#ifndef __MINGW32__
#include <sys/errno.h>  /* for EBADF */
#endif // __MINGW32__
#include "defs.h"
#include "config.h"
#include "config_ipa.h"	/* -INLINE/-IPA group options */
#include "strtab.h"	/* strtab support for Inline_.*_Strtab */
#include "erglob.h"	/* Include the error numbers */
#include "flags.h"	/* Option group processing */
#include "ipa_option.h" /* Trace_IPA */
#include "tracing.h"	/* Tfile */

#include "dwarf_DST_mem.h"          /* for dst */
#include "ipc_defs.h"
#include "ipc_file.h"
#include "ipc_option.h"

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

#include "ipo_main.h"
#include "ipc_partition.h" /* IP_tagged_symbol_partition */
INT number_of_partitions = 1;

#endif /* !_STANDALONE_INLINER */


/* ====================================================================
 *
 * Add_Symbols_To_Strtab
 *
 * Add the symbols in a comma-separated list after "must=" or "never="
 * to appropriate Strtab.
 *
 * ====================================================================
 */

#include <vector>

typedef std::vector <char *, mempool_allocator<char *> > ARRAY_OF_STRINGS;

#define USER_NOT_SPEC          0x0     // User did not specify anything regarding this symbol
#define USER_MUST_INLINE       0x1     // User specified Must inline this pu
#define USER_NO_INLINE         0x2     // User specified Must NOT inline this pu

#define SKIP_NOT_SPEC          0x0     // skip equal to the PU name
#define SKIP_EQUAL             0x1     // skip equal to the PU name

static INLINE_PU_MAP user_inline_request;

static ARRAY_OF_STRINGS must_routines;

static INLINE_PU_MAP skip_request;

struct Hash_Symbols_For_Inlining {
    void operator()(char *symname, UINT act) { 
	user_inline_request[symname] = act; 
        if (act == USER_MUST_INLINE)
	    must_routines.push_back(symname);
    } ;
};

struct Hash_Symbols_For_Skipping {
    void operator()(char *symname, UINT act) { 
	skip_request[symname] = act; 
    } ;
};

static INLINE_EDGE_MAP user_inline_edge_request;

struct Hash_Edges_For_Inlining {
    void operator()(char* edge, UINT act) { 
      mINT32 edge_index = atoi(edge);
      user_inline_edge_request[edge_index] = act; 
    } ;
};


template <class SYM, class INLINE_MAP>
static UINT 
User_Specified_Info(SYM name, INLINE_MAP& request)
{
    typename INLINE_MAP::iterator result = request.find(name);

    if (result != request.end()) {
	return (result->second);
    }
    else 
	return (USER_NOT_SPEC);
}

UINT
User_Specified_Name_Info(char* name)
{
    return User_Specified_Info(name, user_inline_request);
}

UINT
User_Specified_Edge_Info(INT edge_index)
{
    return User_Specified_Info(edge_index, user_inline_edge_request);
}

BOOL
Is_User_Not_Specified(UINT info)
{
    return (info == USER_NOT_SPEC);
}

BOOL
Is_User_Must_Inline(UINT info)
{
    return (info == USER_MUST_INLINE);
}

BOOL
Is_User_No_Inline(UINT info)
{
    return (info == USER_NO_INLINE);
}

static UINT
User_Specified_Skip_Info(char* name)
{
    return User_Specified_Info(name, skip_request);
}

BOOL
Is_Skip_Not_Specified(char *name)
{
    return (User_Specified_Skip_Info(name) == SKIP_NOT_SPEC);
}

BOOL
Is_Skip_Equal(char *name)
{
    return (User_Specified_Skip_Info(name) == SKIP_EQUAL);
}

#ifdef _STANDALONE_INLINER

#include "ipc_type_merge.h"
#include "ipc_utils.h"
#include "inline.h"

static ARRAY_OF_STRINGS filenames;
static ARRAY_OF_STRINGS libnames;
static MEM_POOL Multifile_Pool;
static BOOL multifile_mempool_initialized = FALSE;

struct Process_Specified_Files {
    void operator()(char* filename, UINT unused) { 
	filenames.push_back(filename);
    };
};

struct Process_Specified_Libraries {
    void operator()(char* libname, UINT unused) { 
	libnames.push_back(libname);
    };
};

void
Process_Non_Local_Files()
{
    if (! multifile_mempool_initialized) {
	multifile_mempool_initialized = TRUE;
	MEM_POOL_Initialize (&Multifile_Pool, "TY Merge Pool", 0);
	Initialize_Type_Merging_Hash_Tables (&Multifile_Pool);
    }
	
    for (int i = 0; i < filenames.size(); ++i) {
	Process_Nonlocal_File(filenames[i], NULL);
    }
}


static void
process_archive_member (off_t offset, void* base, void *handle, int fd, char* libname)
{
    struct ar_hdr *header;
    char* name;
    char* member_name;
    void* member_base;


    header = (struct ar_hdr *) ((char *)base + offset);
    name = Read_Member_Name (header, handle, Malloc_Mem_Pool);
    member_name = (char*) MEM_POOL_Alloc(Malloc_Mem_Pool, (strlen (libname) + strlen (name) + 3));
    sprintf (member_name, "%s(%s)", libname, name);

    member_base = (char *)base + offset + sizeof(struct ar_hdr);
    if ((UINT64)member_base & 0xf) {
	// unaligned WHIRL object in an archive 
	UINT64 member_file_size = atoi (header->ar_size);
        member_base = MEM_POOL_Alloc(Malloc_Mem_Pool, member_file_size);
	if (lseek(fd, offset, SEEK_SET) == -1 ||
            	read (fd, member_base, member_file_size) != member_file_size) {
	    fprintf(stderr, "Cannot read %s in %s\n", name, libname);
	    return;
	}
    }

    MEM_POOL_FREE (Malloc_Mem_Pool, name);	/* malloc'ed by read_member_name() */
    
    Process_Nonlocal_File(member_name, member_base);

} /* process_archive_member */

static void
process_archive (int fd, void* base, UINT64 file_size, BOOL allflag, char* libname)
{
    char* sym_name;
    off_t offset = 0;
    void* handle;

    handle = Digest_Archive (base, Malloc_Mem_Pool, file_size);

    if (handle == NULL) return;

    if (allflag) {
	offset = Next_Archive_Member ((char *)base, 0, file_size);
	while (offset) {
	    process_archive_member (offset, base, handle, fd, libname);
	    offset = Next_Archive_Member ((char *)base, offset, file_size);
	}

    } else {
	for (int i = 0; i < must_routines.size(); ++i) {
	
	    if (offset = Defined_By_Archive (must_routines[i], handle)) {
	        process_archive_member (offset, base, handle, fd, libname);
	    }
	}
	    
    }
    Cleanup_Archive_Handle (handle, Malloc_Mem_Pool);

} /* process_archive */

static void
process_library(char *libname)
{
    char *filename;
    void *map_addr;
    int fd;
    struct stat stat_buf;

    fd = open(libname, O_RDONLY, 0);

    if ( (fd == -1) || (fstat(fd, &stat_buf) == -1) ) {
        fprintf(stderr, "Bad library used -- %s\n", libname);
        return;
    }
    map_addr = (char *) mmap ( NULL, stat_buf.st_size,
                           (PROT_READ | PROT_WRITE),
                           MAP_PRIVATE, fd, 0 );

    process_archive(fd, map_addr, stat_buf.st_size, INLINE_All, libname);

}

void
Process_Non_Local_Libraries()
{
    if (! multifile_mempool_initialized) {
	multifile_mempool_initialized = TRUE;
	MEM_POOL_Initialize (&Multifile_Pool, "TY Merge Pool", 0);
	Initialize_Type_Merging_Hash_Tables (&Multifile_Pool);
    }

    for (int i = 0; i < libnames.size(); ++i) {
	process_library(libnames[i]);
    }
}

#endif // _STANDALONE_INLINER

template <class DATA, class ACTION>
static void
Add_Symbols(char *args, DATA data, ACTION perform_action
#ifdef KEY
, const char * opt
#endif
)
{
#ifdef KEY
  if (! args)
  {
    ErrMsg (EC_No_Opt_Val, opt, "INLINE");
    return;
  }
#endif // KEY
  BOOL more_symbols = TRUE;

  do {
    char *endc;

    if ( (endc = strchr(args, ',')) == NULL ) {
      more_symbols = FALSE;
    } 
    else {
      *endc++ = '\0';
    }

    if ( *args != '\0' ) {
      perform_action(args, data);
      args = endc;
    }
  }
  while ( more_symbols );
}


#ifndef __MINGW32__

/* ====================================================================
 *
 * Process_Inline_Option_File
 *
 * Process newline-separated inline options specified in the file with
 * name file_name.  Ignores blank lines, trailing whitespace, and
 * comment lines beginning with '#'.
 *
 * The routine works by converting each non-trivial line to a normal
 * looking -INLINE: option and calling the normal processing.
 *
 * ====================================================================
 */

static void
Process_Option_File ( const char *file_name , const char *type_name)
{
  INT fd;
  struct stat stat_buf;
  char *buffer;

  fd = open ( file_name, O_RDONLY, 0 );
  if ( (fd == -1)
    || (fstat(fd, &stat_buf) == -1) )
  {
    ErrMsg ( EC_Inv_SpecFile, type_name, file_name );
    return;
  }
  buffer = (char *) mmap ( NULL, stat_buf.st_size,
			   (PROT_READ | PROT_WRITE),
			   MAP_PRIVATE, fd, 0 );
  
  while ( buffer != NULL && *buffer != 0 ) {
    char *endc = strchr ( buffer, '\n' );
    char *option = buffer;
    char *cmnt;

    /* Point buffer to next line or make it NULL: */
    buffer = endc ? endc+1 : endc;

    /* Lines beginning with '#' are comments: */
    if ( *option == '#' ) continue;

    /* Terminate line with nul: */
    if ( endc != NULL ) {
      *endc = 0;
    } else {
      endc = strchr ( option, 0 );
    }

    /* Remove trailing comments: */
    cmnt = strchr ( option, '#' );
    if ( cmnt != NULL ) {
      *(endc=cmnt) = 0;
    }

    /* Remove trailing whitespace: */
    cmnt = endc - 1;
    while ( cmnt >= option && ( *cmnt == ' ' || *cmnt == '\t' ) ) {
      *cmnt-- = 0;
    }

    /* If there's anything left, it's an option! */
    if ( *option != 0 ) {
      if ( *option != '-'
	|| ! Process_Command_Line_Group ( option+1,
					  Common_Option_Groups ) )
      {
	ErrMsg ( EC_SpecFile_Opt, option, type_name, file_name );
      }
    }
  }

  close ( fd );
}
#endif /* __MINGW32__ */

/* ====================================================================
 *
 * Process_Inline_Options
 *
 * Finish processing INLINE/IPA options.  Handle any specification
 * files given, and process any must/never options.
 *
 * ====================================================================
 */

void
Process_Inline_Options ( void )
{
  OPTION_LIST *ol;

#ifndef __MINGW32__
  /* Walk the specfile list.  Since new -INLINE:specfile options add
   * the element to the end of the list, this will handle nested
   * references just fine.  We also use the general common group
   * option processing, so any options from any group will be
   * handled.
   */
  for ( ol = INLINE_Spec_Files; ol != NULL; ol = OLIST_next(ol) ) {
    if ( OLIST_val(ol) != NULL ) {
      Process_Option_File ( OLIST_val(ol), "INLINE" );
    }
  }
#endif /* __MINGW32__ */

  /* Walk the list of must/never options: */
#ifdef KEY
// Get rid of some compiler crashes and assertion failures and replace with
// appropriate error messages
  for ( ol = INLINE_List_Names; ol != NULL; ol = OLIST_next(ol) ) {
    if ( strcmp ( OLIST_opt(ol), "must" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Symbols_For_Inlining(), "must" );
    } 
    else if ( strcmp ( OLIST_opt(ol), "never" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_NO_INLINE, Hash_Symbols_For_Inlining(), "never" );
    } 
    else if ( strcmp ( OLIST_opt(ol), "skip" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_NO_INLINE, Hash_Edges_For_Inlining(), "skip" );
    }
    else if ( strcmp ( OLIST_opt(ol), "edge" ) == 0 ) {
      INLINE_None = TRUE;
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Edges_For_Inlining(), "edge" );
    }
    else if ( strcmp ( OLIST_opt(ol), "in_edge" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Edges_For_Inlining(), "in" );
    }
#ifdef _STANDALONE_INLINER
// not yet enabled
    else if ( strcmp ( OLIST_opt(ol), "file" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), NULL, Process_Specified_Files(), "file" );
    }
    else if ( strcmp ( OLIST_opt(ol), "library" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), NULL, Process_Specified_Libraries(), "library" );
    }
#endif // _STANDALONE_INLINER
    else {
	char flag[256];
	sprintf (flag, "INLINE:%s", OLIST_opt(ol));
	ErrMsg (EC_Not_In_Grp, OLIST_opt(ol), "INLINE", flag);
    }
 }
#else
  for ( ol = INLINE_List_Names; ol != NULL; ol = OLIST_next(ol) ) {
    if ( strcmp ( OLIST_opt(ol), "must" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Symbols_For_Inlining());
    } 
    else if ( strcmp ( OLIST_opt(ol), "never" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_NO_INLINE, Hash_Symbols_For_Inlining() );
    } 
    else if ( strcmp ( OLIST_opt(ol), "skip" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_NO_INLINE, Hash_Edges_For_Inlining() );
    }
    else if ( strcmp ( OLIST_opt(ol), "edge" ) == 0 ) {
      INLINE_None = TRUE;
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Edges_For_Inlining() );
    }
    else if ( strcmp ( OLIST_opt(ol), "in_edge" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), USER_MUST_INLINE, Hash_Edges_For_Inlining() );
    }
#ifdef _STANDALONE_INLINER
    else if ( strcmp ( OLIST_opt(ol), "file" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), NULL, Process_Specified_Files() );
    } 
    else if ( strcmp ( OLIST_opt(ol), "library" ) == 0 ) {
      Add_Symbols( OLIST_val(ol), NULL, Process_Specified_Libraries() );
    } 
#endif // _STANDALONE_INLINER
    else {
	FmtAssert ( FALSE,
		   ( "Unknown -INLINE option: %s", OLIST_opt(ol) ) );
    }
 }
#endif // KEY

  /* Check that we don't have conflicting defaults: */
  if ( INLINE_All && INLINE_None ) {
    ErrMsg ( EC_Opt_Conflict, "-INLINE:all", "-INLINE:none",
			      "-INLINE:all" );
    INLINE_None = FALSE;
  }
}

#ifndef __MINGW32__
/* ====================================================================
 *
 * Process_IPA_Skip_Options
 *
 * Finish processing IPA SKIP options.  
 *
 * ====================================================================
 */

void
Process_IPA_Skip_Options ( void )
{
  OPTION_LIST *ol;

  /* Walk the list of skip_... */
  /* only implement skip_equal for now */
  for ( ol = IPA_Skip; ol != NULL; ol = OLIST_next(ol) ) {
    if ( strcmp ( OLIST_opt(ol), "skip_equal" ) == 0 ) {
#ifdef KEY
      Add_Symbols( OLIST_val(ol), SKIP_EQUAL, Hash_Symbols_For_Skipping(), "skip_equal");
#else
      Add_Symbols( OLIST_val(ol), SKIP_EQUAL, Hash_Symbols_For_Skipping());
#endif
    } 
    else {
	FmtAssert ( FALSE,
		   ( "Unknown -skip option: %s", OLIST_opt(ol) ) );
    }
 }

}

#if (!defined(_STANDALONE_INLINER) && !defined(_LIGHTWEIGHT_INLINER))

/*ARGSUSED*/
static void
Add_Symbols_to_Partition_Group ( char *args, INT partition_grp )
{
  DevWarn("Partition groups are not yet ported to new symtab");
}

void
Process_IPA_Specfile_Options ( void )
{
  OPTION_LIST *ol;
  register INT partition_grp = 1;

  /* Walk the specfile list.  Since new -IPA:specfile options add
   * the element to the end of the list, this will handle nested
   * references just fine.  We also use the general common group
   * option processing, so any options from any group will be
   * handled.
   */
  for ( ol = IPA_Spec_Files; ol != NULL; ol = OLIST_next(ol) ) {
    if ( OLIST_val(ol) != NULL ) {
      Process_Option_File ( OLIST_val(ol), "IPA" );
    }
  }

  Process_IPA_Skip_Options();

  /* Walk the list of partition group */
  for ( ol = IPA_Group_Names; ol != NULL; ol = OLIST_next(ol), partition_grp++ ) {
    if ( strcmp ( OLIST_opt(ol), "partition_group" ) == 0 ) {
      Add_Symbols_to_Partition_Group ( strdup(OLIST_val(ol)), partition_grp );
    }
    else {
	FmtAssert ( FALSE,
		   ( "Unknown IPA_Group_Names option: %s", OLIST_opt(ol) ) );
    }
 }
 number_of_partitions = partition_grp;
}

#endif /* !_STANDALONE_INLINER */
#endif /* __MINGW32__ */

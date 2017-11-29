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


// ====================================================================
// ====================================================================
//
// Module: dra_file_util.cxx
//
// Revision history:
//  2-Aug-96: Original Version
//
// Description:
//  File utility routines used in processing .rii files
//
// ====================================================================
// ====================================================================

#include <fcntl.h>              // open()
#include <unistd.h>             // close()
#include <sys/stat.h>           // fstat() 
#ifdef __MINGW32__
#include <windows.h>
#else
#include <sys/mman.h>           // mmap()
#endif /* __MINGW32__ */
#include <sys/types.h>          // off_t, size_t
#include <sys/param.h>          // MAXPATHLEN

#include "defs.h"               // Standard definitions
#include "glob.h"               // Src_File_Name
#include "erbe.h"               // EC_DRA_*
#include "erglob.h"             // EC_No_Mem
#include "file_util.h"          // New_Extension

#include "dra_internal.h"       // Internal DRA interface


// =====================================================================
// 
//                      Exported variables
//
// =====================================================================

INT DRA_file_desc = -1;

char *DRA_file_mmap = NULL;

char DRA_file_name[MAXPATHLEN];

#ifdef __MINGW32__
HANDLE DRA_file_mmap_handle = NULL;
#endif /* __MINGW32__ */

// =====================================================================
// 
//                      Local function prototypes
//
// =====================================================================

static void DRA_Make_File_Name();

static char* get_base_name(char *const s);

static char* get_dir_name(char *const s);


// =====================================================================
// 
//                      File static variables
//
// =====================================================================

static char *DRA_keep_old_file;

static off_t DRA_file_size, DRA_bytes_to_keep;

static const char* DRA_DIRECTORY = "/rii_files/"; // Directory for .rii files

static const char* DRA_FILE_EXTENSION = ".rii";   // Prelinker file extension


// =====================================================================
// 
//                      Exported function definitions
//
// =====================================================================


// =====================================================================
//
// Function Name: DRA_Open_And_Map_File
//
// Description: Open the file and perform memory mapping.
//
// =====================================================================

void 
DRA_Open_And_Map_File()
{
  Set_Error_Phase("Reading prelinker file");

  DRA_Make_File_Name();

  struct stat stat_buf;
  errno = 0;

  DRA_file_desc = open(DRA_file_name, O_RDONLY);

  if (DRA_file_desc < 0 || fstat(DRA_file_desc, &stat_buf) != 0) {
	close(DRA_file_desc);
	ErrMsg(EC_DRA_rii_file_io, DRA_file_name, errno);
	return;
  }

#ifdef __MINGW32__
  DRA_file_mmap = NULL;
  DRA_file_mmap_handle =
	CreateFileMapping((HANDLE) _get_osfhandle(DRA_file_desc), NULL,
			  PAGE_READWRITE, 0, stat_buf.st_size, DRA_file_name);
  if (DRA_file_mmap_handle)
	DRA_file_mmap = (char *) MapViewOfFileEx(DRA_file_mmap_handle,
						 FILE_MAP_COPY,
						 0, 0, stat_buf.st_size, 0);
  if (DRA_file_mmap == NULL) {
	close(DRA_file_desc);
	ErrMsg(EC_DRA_rii_file_io, DRA_file_name, GetLastError());
	return;
  }
#else
  DRA_file_mmap = (char *) mmap(0, 
                                stat_buf.st_size, 
                                PROT_READ|PROT_WRITE, 
                                MAP_PRIVATE, 
                                DRA_file_desc, 
                                0);

  if (DRA_file_mmap == (char *)(MAP_FAILED)) {
	close(DRA_file_desc);
	ErrMsg(EC_DRA_rii_file_io, DRA_file_name, errno);
	return;
  }
#endif /* __MINGW32__ */

  close(DRA_file_desc);

  // If everything is fine, save the size of the .rii file
  //
  DRA_file_size = stat_buf.st_size;

  // Find the size of the first two sections of the .rii file
  // CMDLINE, PWD
  // ----
  // Cloning requests
  // ----
  // Common layout info
  //
  char *first_sep = strstr(DRA_file_mmap, DRA_FILE_SEPARATOR);

  // The first separator should always be there
  //
  if (first_sep == NULL) {
    (void) unlink(DRA_file_name);
    ErrMsg(EC_DRA_rii_file_format, DRA_file_name);
	return;
  }
  
  char *second_sep = strstr(first_sep+1, DRA_FILE_SEPARATOR);

  if (second_sep == NULL) {
    DRA_bytes_to_keep = DRA_file_size;
  }
  else {
    DRA_bytes_to_keep = second_sep - DRA_file_mmap;
  }

  DRA_keep_old_file = CXX_NEW_ARRAY(char, DRA_bytes_to_keep, Malloc_Mem_Pool);
  if (DRA_keep_old_file == NULL) {
    ErrMsg(EC_No_Mem, "DRA_Open_And_Map_File");
	return;
  }
  else {
    (void) strncpy(DRA_keep_old_file, DRA_file_mmap, DRA_bytes_to_keep);
  }
}




// =====================================================================
//
// Function Name: DRA_Set_Write_Location
//
// Description: Find the place in .rii file where information
//              about common blocks should be written.
//
// =====================================================================

void
DRA_Set_Write_Location(void)
{
  errno = 0;

  DRA_file_desc = open(DRA_file_name, O_WRONLY|O_TRUNC);

  if (DRA_file_desc < 0) {
	close(DRA_file_desc);
	ErrMsg(EC_DRA_rii_file_io, DRA_file_name, errno);
	return;
  }

  write(DRA_file_desc, (void*)DRA_keep_old_file, DRA_bytes_to_keep);
  write(DRA_file_desc, (void*)DRA_FILE_SEPARATOR, strlen(DRA_FILE_SEPARATOR));
  
  CXX_DELETE_ARRAY(DRA_keep_old_file, Malloc_Mem_Pool);
}




// =====================================================================
//
// Function Name: DRA_Mem_Unmap_File
//
// Description: Unmap the memory associated with the prelinker file
//
// =====================================================================

void 
DRA_Mem_Unmap_File()
{
#ifdef __MINGW32__
  (void) UnmapViewOfFile(DRA_file_mmap);
  (void) CloseHandle(DRA_file_mmap_handle);
#else
  (void) munmap((void *)DRA_file_mmap, (size_t)DRA_file_size);
#endif /* __MINGW32__ */
}




// =====================================================================
//
// Function Name: DRA_Close_File
//
// Description: Close .rii file
//
// =====================================================================

void 
DRA_Close_File()
{
  if (DRA_file_desc >= 0) {
    close(DRA_file_desc);
  }
}




// ======================================================================
// 
// The following functions were lifted from the prelinker (edg_prelink.c)
//
// ======================================================================

// ======================================================================
// 
// Function Name: Make_DRA_File_Name
//
// Description: Build the instantiation info filename. It mimics the 
//              "make_ii_file_name" routine in edg_prelink. The filename 
//              we build is: dirname(obj)/rii_files/basename(obj,.o).rii
//              The name is written into file static buffer rii_file_name
//
// ======================================================================

static void 
DRA_Make_File_Name()
{
  char *obj_file_name = Obj_File_Name ? 
    Obj_File_Name : New_Extension (Src_File_Name, ".o");
  
  char *dir = get_dir_name(obj_file_name);
  strcpy (DRA_file_name, dir);
  strcat (DRA_file_name, DRA_DIRECTORY);

  char *base = get_base_name(obj_file_name);
  INT baselen = strlen(base);
  
  if (base[baselen-2] == '.' && base[baselen-1] == 'o')
    strcpy(&base[baselen-2], DRA_FILE_EXTENSION);
  else
    strcpy(&base[baselen], DRA_FILE_EXTENSION);

  strcat (DRA_file_name, base);
} 




static char tempbuf[MAXPATHLEN];


static char*
get_base_name(char *const s)
{
  register char *p;
  register char *const t = tempbuf;

  if (s == NULL || *s == 0) {
    return strcpy(t, ".");
  } else {
    p = strcpy(t, s);
    p += strlen(p);
    while( p != t  &&  *--p == '/' )        // skip trailing /s 
      *p = '\0';
    while( p != t ) {
      if( *--p == '/' )
        return  ++p;
    }
    return p;
  }
}


static char*
get_dir_name(char *const s)
{
  register char *p;
  register char *const t = tempbuf;

  if (s == NULL || *s == 0) {
	return strcpy(t, ".");
  } else {
	p = strcpy(t, s);
	p += strlen(p);
    while( p != t  &&  *--p == '/' )        // skip trailing /s 
      ;

    if ( p == t && *p == '/' )
      return strcpy(t, "/");

    while( p != t ) {
      if( *--p == '/' ) {
		if ( p == t )
          return strcpy(t, "/");
		while ( *p == '/' )
          p--;
		*++p = '\0';
		return  t;
      }
	}
	return strcpy(t, ".");
  }
}

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/*

Given an fd, open up an xlate consumer handle

xlate_init_fd.c

$Revision: 1.1.1.1 $
$Date: 2005/10/21 19:00:00 $

*/
#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_init_fd_xtnd  = _xlate_init_fd_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_init_fd  = _xlate_init_fd
#endif

int
xlate_init_fd(int fd, int open_debug_table,
	xlate_table_con * ret_tab_ptr)
{

   Elf *elf;
   xlate_table_con ltab = 0;
   int retstatus  = XLATE_TB_STATUS_NO_ERROR;

   if(elf_version(EV_CURRENT) == EV_NONE) {
	return XLATE_TB_STATUS_ELF_VERSION_BAD;
   }

#if defined(linux) || defined(BUILD_OS_DARWIN)
   elf = elf_begin(fd,ELF_C_READ,NULL);
#else
   elf = elf_begin(fd,ELF_C_READ_MMAP,NULL);
#endif
   if(elf == NULL) {
	return XLATE_TB_STATUS_ELF_BEGIN_BAD;
   }

   retstatus = xlate_init_elf(elf,
		open_debug_table,&ltab);

   if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
       (void)elf_end(elf);
   } else {
	ltab->xc_did_elf_begin = 1;
	*ret_tab_ptr = ltab;
   }

   return retstatus;
}

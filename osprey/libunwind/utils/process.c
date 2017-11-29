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



#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <sys/unwind.h>



/* extern definitions */
extern __unw_table_entry_t *_unwind_table;
extern __uint64_t _unwind_table_size;
extern __unw_info_t *_unwind_info;
extern __uint64_t _unwind_info_size;



/* function to process the unwind table and/or the unwind info */
__unw_error_t unwind_process(
	__unw_error_t (*func)(char *, __uint64_t, char *, __uint64_t, void *),
	void *arg) 
{
        return (__unw_error_t)((*func)((char *)_unwind_table,
                _unwind_table_size * sizeof(__unw_table_entry_t),
                (char *)_unwind_info, _unwind_info_size, arg));
}

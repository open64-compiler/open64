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


#ifndef dwarf_DST_dump_INCLUDED
#define dwarf_DST_dump_INCLUDED



#ifdef _KEEP_RCS_ID
static char *dwarf_DST_dump_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "dwarf_DST.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Saves the dump file name for use in the DST_dump() routine!
*/
extern void DST_set_dump_filename(char *file_name);


/* The main dumping routine!
*/
extern void DST_dump(DST_DIR_IDX  incl_dirs,
		     DST_FILE_IDX files,
		     DST_INFO_IDX compile_unit);

/* Alternate entry to dump routine:
 * finds idx values implicitly;
 * passes in the file to dump to, == stdout if f == NULL.
 */
extern void Dump_DST (FILE *f);

#ifdef __cplusplus
}
#endif
#endif /* dwarf_DST_dump_INCLUDED */

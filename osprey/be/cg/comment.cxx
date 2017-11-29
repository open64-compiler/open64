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


/* =======================================================================
 * =======================================================================
 *
 *  Module: comment.cxx
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:23-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.comment.cxx $
 *
 *  Revision comments:
 *
 *  21-Jan-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Support for arbitrary textual comments.
 *
 * =======================================================================
 * =======================================================================
 */

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "config_asm.h"

#include "mempool.h"
#include "strtab.h"
#include "cgir.h"
#include "note.h"

#include "comment.h"

typedef struct note_info {
  STR_IDX text;
} NOTE_INFO;


#define NOTE_INFO_text(info)	((info)->text)


/* =======================================================================
 *
 *  COMMENT_Handler
 *
 *  Handler for comments.
 *
 * =======================================================================
 */
static void
COMMENT_Handler(
  NOTE_ACTION action,
  NOTE_INFO  *info,
  FILE       *file
)
{
  switch (action) {
  case NOTE_PRINT_TO_FILE:
    fprintf(file, "%s %s\n", ASM_CMNT_LINE, Index_To_Str(NOTE_INFO_text(info)));
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file, "COMMENT_Handler");
    break;
  case NOTE_PRINT_TO_ANL_FILE:
    /* ignore for now */
    break;
  }
}


/* =======================================================================
 *
 *  COMMENT_Add
 *
 *  See interface description.
 *  Store string idx rather than string itself cause
 *  strtab can be realloc'ed.
 *
 * =======================================================================
 */
void
COMMENT_Add(
  BB *bb,
  STR_IDX text
)
{
  NOTE_INFO *info = TYPE_MEM_POOL_ALLOC(NOTE_INFO, &MEM_pu_nz_pool);
  NOTE_INFO_text(info) = text;
  NOTE_Add_To_BB(bb, COMMENT_Handler, info);
}

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
 *  Module: comment.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:23-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.comment.h $
 *
 *  Revision comments:
 *
 *  21-Jan-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Add arbitrary comments to a basic block.
 *
 *  Exported functions:
 *  ===================
 *
 *      void COMMENT_Add(
 *        BB   *bb,
 *	  STR_IDX  text
 *      )
 *
 *          Add comment <text> to <bb>.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef COMMENT_INCLUDED

#ifdef _KEEP_RCS_ID
static const char swp_note_rcs_id[] = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.comment.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

extern void COMMENT_Add(BB *bb, STR_IDX text);

#endif

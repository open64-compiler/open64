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
 *  Module: annotations.c
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:19-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.annotations.cxx $
 *
 *  Description:
 *  ============
 *
 *  Module to create and access annotations that can be attached to 
 *  any arbitrary data structure.
 *
 *  See annotations.h for description.
 *
 * =======================================================================
 * =======================================================================
 */

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "cgir.h"
#include "annotations.h"


extern ANNOTATION *
ANNOT_Add (
  ANNOTATION *annot_list, 
  ANNOTATION_KIND kind, 
  void *information,
  MEM_POOL *pool)
{
  ANNOTATION *new_a = TYPE_MEM_POOL_ALLOC (ANNOTATION, pool);
  ANNOTATION *list, *next;

  ANNOT_next(new_a) = NULL;
  ANNOT_info(new_a) = information;
  ANNOT_kind(new_a) = kind;

  /* The new annotation is added at the end of the annotations list.
   * This maintains the order in which annotations are added. Some 
   * clients care about this.
   */
  for (list = annot_list; list != NULL; list = next) {
    next = ANNOT_next(list);
    if (next == NULL) {
      ANNOT_next(list) = new_a;
      break;
    }
  }
  if (annot_list == NULL) annot_list = new_a;
  return annot_list;

}

extern ANNOTATION *
ANNOT_Unlink (
  ANNOTATION *annot_list,
  ANNOTATION *this1)
{
  ANNOTATION *list, *next;

  if ( annot_list == this1 ) {
    return ANNOT_next(annot_list);
  }

  for (list = annot_list; list != NULL; list = next) {
    next = ANNOT_next(list);
    if ( next == this1 ) {
      ANNOT_next(list) = ANNOT_next(next);
      break;
    }
  }

  return annot_list;
}

extern ANNOTATION *
ANNOT_Get (ANNOTATION *list, ANNOTATION_KIND kind)
{
  ANNOTATION_KIND cur_kind;

  while (list != NULL) {
    cur_kind = ANNOT_kind(list);
    if (cur_kind == kind) {
      break;
    }
    list = ANNOT_next(list);
  }
  return list;
}


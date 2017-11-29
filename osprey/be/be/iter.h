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
 * Module: iter.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:18-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/be/SCCS/s.iter.h $
 *
 * Revision history:
 *  03-MAR-96 - Original Version
 *
 * Description:
 *  header for back-end driver iterators
 *
 * ====================================================================
 * ====================================================================
 */
#ifndef iter_INCLUDED
#define iter_INCLUDED

#ifdef _KEEP_RCS_ID
static char *be_utilrcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/be/SCCS/s.iter.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

/* PU loop iterator macros */
#define PU_INFO_ITER_stack_size	10
#define PU_INFO_ITER_current(x)		((x)->current_pu)
#define PU_INFO_ITER_sp(x)		((x)->sp)
#define PU_INFO_ITER_stack(x)		((x)->stack[(x)->sp])
#define PU_INFO_ITER_is_top_level(x)	((x)->current_pu == NULL || \
					 (x)->sp == 0)

/* PU loop iterator */
typedef struct pu_info_iter {
  PU_Info *current_pu;
  PU_Info *stack[PU_INFO_ITER_stack_size];
  INT32 sp;
} PU_INFO_ITER;

/* PU loop iterator prototypes */
void Pu_Init(PU_INFO_ITER *, PU_Info *);
BOOL Pu_While(PU_INFO_ITER *);
void Pu_Next(PU_INFO_ITER *);

#ifdef __cplusplus
}
#endif

#endif /* be_iter_INCLUDED */

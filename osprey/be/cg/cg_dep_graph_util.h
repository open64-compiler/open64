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
 *  Module: cg_dep_graph_update.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:20-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_dep_graph_util.h $
 *
 *  Revision comments:
 *
 *  3-Aug-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef CG_DEP_GRAPH_UTIL
#define CG_DEP_GRAPH_UTIL


inline OP_LIST* CG_DEP_Get_Defs(TN *tn, TN_MAP usage_map){
  return (OP_LIST *) TN_MAP_Get(usage_map, tn);
}

inline void CG_DEP_Add_Def(OP *op, UINT8 pos, TN_MAP usage_map, MEM_POOL *pool){
  OP_LIST *defs_list = CG_DEP_Get_Defs(OP_result(op, pos), usage_map); 
  TN_MAP_Set(usage_map, OP_result(op, pos), OP_LIST_Push(op, defs_list, pool));
}

inline void CG_DEP_Add_Def_By_TN(OP* op, TN* result_tn, TN_MAP usage_map, MEM_POOL *pool){
  OP_LIST *defs_list = CG_DEP_Get_Defs(result_tn, usage_map); 
  TN_MAP_Set(usage_map, result_tn, OP_LIST_Push(op, defs_list, pool));
}

#endif /* CG_DEP_GRAPH_UTIL */

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


// -*-C++-*-
//                  	LNO DOACROSS Parallelization
//                  	----------------------------
//

/* ====================================================================
 * ====================================================================
 *
 * Module: doacross.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Parallelize loops with loop-carried dependences
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef doacross_INCLUDED
#define doacross_INCLUDED

#define NULL_DIST INT_MAX

extern WN* Parallelize_Doacross_Loop(
			WN* processor_loop,
			WN* processor_tile_loop,
			INT32 Doacross_Tile_Size,
			INT32 Sync_Distances[2],
			ARRAY_DIRECTED_GRAPH16* dg,
			DU_MANAGER* du);

extern void Doacross_Init(
			WN* func_nd);

extern void Doacross_Finish();

extern void Compute_Sync_Distances(
			WN* wn_outer,
                        INT nloops,
			INT permutation[],
                        INT parallel_depth,
                        SNL_DEP_MATRIX** sdm_inv,
			BOOL retained[],
                        INT sync_distances[2]);

extern INT Get_Doacross_Tile_Size(
                        INT sync_distances[],
                        WN* wn_outer,
                        INT permutation[],
                        INT nloops,
                        INT parallel_depth,
                        INT num_procs,
                        double work_estimate);

extern double Compute_Doacross_Delay_Cycle(
                        WN* wn_outer,
                        INT permutation[],
                        INT parallel_depth,
                        INT num_proc,
                        INT doacross_tile_size,
			INT sync_distances[],
			double machine_cycles);

extern double Compute_Doacross_Sync_Cycle(
                        WN* wn_outer,
                        INT permutation[],
                        INT parallel_depth,
                        INT doacross_tile_size,
                        INT sync_distances[]);

extern BOOL Dep_Preserved(
                        DEPV* depv,
                        INT doacross_dim,
                        INT sync_distances[2]);

extern BOOL Check_Doacross_Sync_Coverage(
                        WN* doacross_loop,
                        INT sync_distances[2]);

#endif // doacross_INCLUDED

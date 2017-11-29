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

#ifndef wind_down_INCLUDED
#define wind_down_INCLUDED "wind_down.h"

/**
*** Description:
***
*** Exported Functions:
***
***	void Wind_Down_Dep_V(WN* orig, WN* copy, HASH_TABLE<VINDEX16,
***	  VINDEX16>* hash_table, ARRAY_DIRECTED_GRAPH16* dg)
***
***	  Create the vertices of the dependence graph of the wind down
***	  loop 'copy' given those of the original loop 'orig'.  Use the 
***	  'hash_table' to collect pairs corresponding vertices from the 
***	  original loop and wind down loop.
***
***	BOOL Wind_Down_Dep_E(HASH_TABLE<VINDEX16,VINDEX16>* hash_table,
***	  INT d, ARRAY_DIRECTED_GRAPH16* dg)
***
***	  Create the edges of the dependence graph for the original
***	  loop and wind down loop which go between the pairs of correspon-
***	  ding vertices in the 'hash_table'.  The original loop for the 
***	  which the wind down loop was created has depth 'd'.  Returns
***	  TRUE if all the edges were put in successfully, FALSE otherwise.
***
***	void Set_Winddown_Annotations(WN* newloop, BOOL cache_winddown,
***	  EST_REGISTER_USAGE  est_register_usage, BOOL outer)
***
***	  Set the annotations of the wind down loop 'newloop'.  If this
***	  is a cache winddown loop 'cache_winddown' is TRUE, otherwise it
***	  is a register winddown loop.  The estimated register usage of 
***	  the new loop is given by 'est_register_usage'.  If this is not an 
***	  innermost loop, 'outer' is TRUE, otherwise it is FALSE.
***
***	WN* Wind_Down(WN* loop, INT est_num_iterations, BOOL cache_winddown, 
***	  EST_REGISTER_USAGE est_register_usage)
***
***	  Create a wind down loop for the loop 'loop'.  If 'cache_winddown'
***	  is TRUE, annotate it as a cache winddown loop, otherwise, anno-
***	  tate it as a register winddown loop.  Mark the windown as having    
***	  the 'est_num_iterations' iterations.  Assign it an estimated
***	  register usage given by 'est_register_usage'.
***
**/ 

extern void Wind_Down_Dep_V(WN* orig, WN* copy, HASH_TABLE<VINDEX16,
  VINDEX16>* hash_table, ARRAY_DIRECTED_GRAPH16* dg);

extern BOOL Wind_Down_Dep_E(HASH_TABLE<VINDEX16,VINDEX16>* hash_table,
  INT d, ARRAY_DIRECTED_GRAPH16* dg);

extern void Set_Winddown_Annotations(WN* newloop, BOOL cache_winddown,
  EST_REGISTER_USAGE  est_register_usage, BOOL outer);

extern WN* Wind_Down(WN* loop, INT est_num_iterations, BOOL cache_winddown,
  EST_REGISTER_USAGE est_register_usage);

#endif /* wind_down_INCLUDED */

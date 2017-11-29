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

/**
***	Description:  This file contains functions used to do lego and mp 
***		      tiling. 
***
***     WN* Lego_Tile_Loop(WN* loop,
***                        MEM_POOL *pool)
***
***          Processor tile (pseudo-lower) the 'loop' according to the infor-
***          mation in the LEGO_INFO.  Memory for data structures created
***          during the tiling is allocated from 'pool'.
***
***     WN* Mp_Tile_Loop(WN* loop,
***			 BOOL LNO_Ozero, 
***                      MEM_POOL *pool)
***
***          Processor tile (pseudo-lower) the 'loop' according to the infor-
***          mation in the MP_INFO.  Memory for data structures created
***          during the tiling is allocated from 'pool'. Set 'LNO_Ozero' 
***	     to TRUE if you are tiling at -O0, at -O3 set it to FALSE. 
***
***     void Lego_Mp_Tile(WN* wn_root, BOOL LNO_Ozero)
***
***         Processor tile (pseudo-lower) all of the loops with MP_INFO 
***	    and/or LEGO_INFO according to the information in that MP_INFO
***	    and/or LEGO_INFO.
***
***     void Lego_Tile(WN* wn_root, BOOL LNO_Ozero)
***
***         Processor tile (pseudo-lower) all of the loops with LEGO_INFO
***         according to the information in that LEGO_INFO.
***
***	void Mp_Tile(WN* wn_root)
***
***	    Processor tile (pseudo-lower) all of the loops which have been 
***	    marked for parallelization. 
***
***	void Repair_Bad_Dependences(WN* wn_loop)
***
***	    Repair the lexicographically negative dependences incident on
***	    nodes inside the loop 'wn_loop' by recomputing them.
***
***     void Is_Versioned_Mp_Loop(WN* wn_loop) 
***
***         Return TRUE if 'wn_loop' is a versioned MP loop, FALSE if it
***         is not.
***
***     BOOL Mp_Want_Version_Loop(WN* wn_loop, BOOL test_already)
***
***         Returns TRUE if the DOACROSS loop 'wn_loop' showed be ver-
***         sioned in LNO, returns FALSE otherwise.  If 'test_already',
***         return FALSE if we have already versioned. 
***
**/

extern WN* Lego_Tile_Loop(WN* loop,
                          MEM_POOL *pool);                  

extern WN* Mp_Tile_Loop(WN* loop,
			BOOL LNO_Ozero, 
                        MEM_POOL *pool);

extern void Lego_Mp_Tile(WN* wn_root,
			 BOOL LNO_Ozero);

extern void Lego_Tile(WN* wn_root,
		      BOOL LNO_Ozero);

extern void Mp_Tile(WN* wn_root); 

extern void Repair_Bad_Dependences(WN* wn_loop);

extern BOOL Is_Versioned_Mp_Loop(WN* wn_loop);

extern BOOL Mp_Want_Version_Loop(WN* wn_loop, BOOL test_already);


/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipc_pu_size_INCLUDED
#define ipc_pu_size_INCLUDED

// Estimated weight of a PU used by the inlining heuristic.  Weight is
// based on basic block count (bb_count), statement count (stmt_count), and
// number of function calls made (call_count).
inline UINT32
PU_Weight (UINT32 bb_count, UINT32 stmt_count, UINT32 call_count)
{
    return bb_count * 5 + stmt_count + call_count;
}

// Size of a PU.  These values are used to compute the "weight" of a PU
// used by the inlining heuristic
class PU_SIZE
{
private:
  UINT32 _bb_count;		    // basic block count
  UINT32 _stmt_count;		    // statement count
  UINT32 _call_count;		    // (call_count == 0) implies leaf
  
public:
  // constructor
  PU_SIZE (UINT32 bb=0, UINT32 stmt=0, UINT32 call= 0) :
    _bb_count (bb), 
    _stmt_count (stmt),
    _call_count (call)
  {}
    
  UINT32 Call_Count () const 	{ return _call_count; };
  UINT32 Bb_count() const 		{ return _bb_count;}
  UINT32 Stmt_count() const 	{ return _stmt_count;}

  void Set_PU_Size (UINT32 bb, UINT32 stmt, UINT32 call) 
  {
    _bb_count = bb;
    _stmt_count = stmt;
    _call_count = call;
  }

  void operator= (const PU_SIZE& p) 
  {
    _bb_count = p._bb_count;
    _stmt_count = p._stmt_count;
    _call_count = p._call_count;
  }

  void Inc_PU_Size (INT32 bb, INT32 stmt, INT32 call) 
  {
    _bb_count += bb;
    _stmt_count += stmt;
    _call_count += call;
  }

  void operator+= (const PU_SIZE& p) 
  {
    _bb_count += p._bb_count;
    _stmt_count += p._stmt_count;
    _call_count += p._call_count;
  }

  // give function with lots of bb lower priority by increasing its weight
  UINT32 Weight () const 
  {
    return PU_Weight (_bb_count, _stmt_count, _call_count);
  }

  UINT32 Olimit () const 
  {
    return COMPUTE_OLIMIT (_bb_count, _stmt_count);
  }

}; // PU_SIZE

#endif // ipc_pu_size_INCLUDED

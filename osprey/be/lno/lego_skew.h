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
***  Exported types and functions:
***
***	void Lego_Skew_Indices(WN* wn_tree)
***
***	  If all of the reshaped arrays in a loop have a constant offset
***	  "k", skew the loop indices to remove the constant offset.  
**/

class LEGO_SKEW {
private: 
  ACCESS_VECTOR* _av; 
  WN* _array; 
  INT _dim; 
  INT _count; 
public: 
  ACCESS_VECTOR* Av() { return _av; }
  WN* Array() { return _array; }
  INT Dim() { return _dim; } 
  INT Count() { return _count; }  
  void Increment() { _count++; }
  LEGO_SKEW(ACCESS_VECTOR* av, WN* array, INT dim, INT count) 
    : _av(av), _array(array), _dim(dim), _count(count) {} 
  ~LEGO_SKEW() {}  
}; 

extern void Lego_Skew_Indices(WN* wn_tree); 


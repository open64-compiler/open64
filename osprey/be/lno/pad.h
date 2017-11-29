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
#ifndef pad_INCLUDED
#define pad_INCLUDED
#define USE_STANDARD_TYPES
#ifndef defs_INCLUDED
#include "defs.h"
#endif

#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif


//======================================================
// 
// And array describing the bounds information
//
//======================================================
class BOUNDS
{
private:
 static const UINT8 IS_CONSTANT_BOUNDS = 0x1;
 INT64 _upper, _lower, _stride;
 UINT8 _flags;

public:
 void Init() { _upper = _lower = _stride = 0;};
 void Init(INT64 upper, INT64 lower, INT64 stride) {
    _upper = upper; _lower = lower; _stride = stride;
  };

 void Set_Constant() { _flags = _flags | IS_CONSTANT_BOUNDS; };
 BOOL Is_Constant() { return _flags & IS_CONSTANT_BOUNDS;};

 INT64 Get_Upper() { return _upper;};
 INT64 Get_Lower() { return _lower;};
 INT64 Get_Stride() { return _stride;};
 void Set_Upper(INT64 upper) { _upper = upper;};
 void Set_Lower(INT64 lower) { _lower = lower;};
 void Set_Stride(INT64 stride) { _stride = stride;};

};

typedef DYN_ARRAY<BOUNDS> BOUNDS_ARRAY;


#endif

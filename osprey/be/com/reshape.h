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


#ifndef reshape_INCLUDED
#define reshape_INCLUDED "ipa_reshape.h"

class RESHAPE
{

private:
  PROJECTED_REGION *_callee_proj_region;
  PROJECTED_REGION *_callsite_region;
  PROJECTED_REGION *_caller_shape_proj_region;
  PROJECTED_REGION *_callee_shape_proj_region;
  PROJECTED_REGION *_callee_proj_reshaped_region;
  MEM_POOL* _m;
 
public:

  BOOL Constant_Type_Reshape();

  PROJECTED_REGION* Reshape_Constant_Shape(BOOL trace = FALSE);

  RESHAPE(PROJECTED_REGION* caller_shape_proj_region,
    PROJECTED_REGION* callee_shape_proj_region,
    PROJECTED_REGION* callee_proj_region,
    PROJECTED_REGION* callsite_region,
    MEM_POOL *m,
    BOOL trace = FALSE);

  PROJECTED_REGION* Reshape_Callee_To_Caller(BOOL trace = FALSE);
                                                            
  BOOL Reshapeable_Passed_Section(BOOL trace = FALSE);
                                                            
  void Reshape_Passed_Section(PROJECTED_REGION* pr, BOOL trace = FALSE);

  void Set_callee_proj_reshaped_region(PROJECTED_REGION* pr) 
    {_callee_proj_reshaped_region  = pr;};

  ~RESHAPE() {};
};

#endif /* reshape_INCLUDED */


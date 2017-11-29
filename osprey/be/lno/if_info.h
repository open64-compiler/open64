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


//-*-c++-*-
//-----------------------------------------------------------------------------
//    IF_INFO
//              Contains information about block structured IFs
//      
//      BOOL Contains_Do_Loops
//
//              There is a DO loop inside this IF
//
//      BOOL Contains_Regions
//
//              There is a region inside this IF
//
//      IF_INFO(MEM_POOL *pool, BOOL contains_do_loops, BOOL contains_regions) 
//
//      ACCESS_ARRAY *Condition
//
//              An ACCESS_ARRAY representation for the condition of the if
//
//      BOOL Codition_On_Then
//
//              Is the access vector a condition for the 'then' or for
//              the else
//
//      MEM_POOL *Pool()
//
//              Which pool was used for this info
//
//      void Print(FILE *fp, INT indentation = 0)
//
//              Print out the info.  Can specify requested whitespace before
//              each line.
//
//-----------------------------------------------------------------------------

#ifndef if_info_INCLUDED
#define if_info_INCLUDED

#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif

class ARA_LOOP_INFO; 

class  IF_INFO
{
  MEM_POOL *_pool;
public:
  IF_INFO(MEM_POOL *pool, BOOL contains_do_loops, BOOL contains_regions) { 
    _pool = pool;
    Contains_Do_Loops = contains_do_loops; 
    Contains_Regions = contains_regions; 
    Condition = NULL;
    Freq_True = -1.0;
    Freq_False = -1.0;
    _ara_then = NULL;
    _ara_else = NULL;
    _ara_common = NULL;
  }

  IF_INFO(IF_INFO *ii, MEM_POOL *pool)
  {
    _pool = pool;
    Condition = CXX_NEW(ACCESS_ARRAY(ii->Condition,pool),pool);
    Contains_Do_Loops = ii->Contains_Do_Loops;
    Contains_Regions = ii->Contains_Regions;
    Condition_On_Then = ii->Condition_On_Then;
    Freq_True = ii->Freq_True;
    Freq_False = ii->Freq_False;
    _ara_then = ii->ARA_then();
    _ara_else = ii->ARA_else();
    _ara_common = ii->ARA_common();
  }

  MEM_POOL *Pool() { return _pool; };
  ACCESS_ARRAY *Condition;
  mBOOL Contains_Do_Loops;
  mBOOL Contains_Regions;
  mBOOL Condition_On_Then;
  float Freq_True;
  float Freq_False;
  void Print(FILE *fp) __attribute__((weak));
  ARA_LOOP_INFO *_ara_then;
  ARA_LOOP_INFO *_ara_else;
  ARA_LOOP_INFO *_ara_common;

  // member access functions
  void Set_ARA_then(ARA_LOOP_INFO *a)  { _ara_then  = a; }
  void Set_ARA_else(ARA_LOOP_INFO *a) { _ara_else = a; }
  void Set_ARA_common(ARA_LOOP_INFO *a){ _ara_common = a;}
  ARA_LOOP_INFO * ARA_then(void)   const { return _ara_then; }
  ARA_LOOP_INFO * ARA_else(void)  const { return _ara_else; }
  ARA_LOOP_INFO * ARA_common(void) const { return _ara_common; }
  
};

#endif

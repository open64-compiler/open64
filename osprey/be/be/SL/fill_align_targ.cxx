/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include "defs.h"
#include "errors.h"
#include "config_targ.h"
#include "config_platform.h"

void Fill_Align_Initialize_Parameters(INT *L1_sz, INT *L2_sz, INT *pg_sz)
{
  INT L1_cache_line_sz;
  INT L2_cache_line_sz;

  switch (Target) {

    case TARGET_sb1:
      L1_cache_line_sz = 32;
      L2_cache_line_sz = 32;
      break;

    default:
      L1_cache_line_sz = 32;
      L2_cache_line_sz = 128;
      break;
  }
  
  *L1_sz = L1_cache_line_sz;
  *L2_sz = L2_cache_line_sz;
  *pg_sz = 16384;
}


/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 *  CG driver declarations which include target dependencies.
 *
 *  THIS FILE IS ONLY TO BE INCLUDE BY ../cgdriver.cxx!!!!
 *
 * ====================================================================
 * ====================================================================
 */

inline BOOL Enable_Prefetch_Ahead_For_Target(void)
{
  return TRUE;
}

inline BOOL Target_Has_Prefetch(void)
{
  return TRUE;
}

inline BOOL Enable_Prefetch_For_Target(void)
{
  return TRUE;
}

inline BOOL Enable_Idiv_In_FPU_For_Target(void)
{
  return FALSE;
}

inline BOOL Target_Has_Cmoves(void)
{
  return TRUE;
}

inline BOOL Enable_Spec_Idiv_For_Target(void)
{
  return FALSE;
}

inline BOOL Enable_Fold_Expanded_daddiu_For_Target(void)
{
  return FALSE;
}

inline BOOL Enable_LOH_For_Target(void)
{
  return FALSE;
}

inline BOOL Enable_Fill_Delay_Slots_For_Target(void)
{
  if (Is_Target_x86_64())
    return TRUE;
  else
    // SGI assembler does not accept something like 
    //     bne $2,$0,.Lt_0_1+4
    // It expects something like 
    //     bne $2,$0,.+76
    // Because, we can not generate absolute addresses (Inline assembly),
    // we do not enable Fill_Delay_Slot optimization for R10k.
    return FALSE;
}

#ifdef PAIRS
inline BOOL Enable_SWP_Memory_Pairs(void)
{
  return FALSE;
}
#endif /* PAIRS */

inline BOOL Enable_SWP_Optimistic_II_Search(void)
{
  return TRUE;
}

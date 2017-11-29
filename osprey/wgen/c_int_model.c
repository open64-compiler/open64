/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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



/******************************************************************************
*                                                             \  ___  /       *
*                                                               /   \         *
* Rat's hacking up stuff for c int data type models
*                                                               \   /         *
*                                                                             *
******************************************************************************/
/*

c_int_model.c -- c data type models

*/

#include "ansidecl.h"	// For definition of PARAMS
#include "defs.h"
#include "c_int_model.h"
#include "config_targ.h"
#ifdef TARG_MIPS 
// #include "gnu_config.h"
extern BOOL TARGET_64BIT;           /* from main.c */
#include "config.h"	/* for Target_Byte_Sex */
#endif

TARGET_INT_MODEL Target_Int_Model = TARGET_INT_ILP32;

#if defined(CFE) || defined(FFE)
/* New variables */

struct {
  TYPE_ID mtype_short;
  TYPE_ID mtype_int;
  TYPE_ID mtype_long;
  TYPE_ID mtype_ushort;
  TYPE_ID mtype_uint;
  TYPE_ID mtype_ulong;
#ifdef longlong
  TYPE_ID mtype_long_long;
  TYPE_ID mtype_ulong_long;
#endif
  TYPE_ID mtype_pointer;
} C_Int_Model[3] =
{ 
  /* ILP32 */
  {MTYPE_I2, /* short */
   MTYPE_I4, /* int */
   MTYPE_I4, /* long */
   MTYPE_U2, /* ushort */
   MTYPE_U4, /* uint */
   MTYPE_U4, /* ulong */
#ifdef longlong
   MTYPE_I8, /* long long */
   MTYPE_U8, /* unsignedlong long */
#endif
   MTYPE_U4 /* pointer */
  },
  /* LP64 */
  {MTYPE_I2, /* short */
   MTYPE_I4, /* int */
   MTYPE_I8, /* long */
   MTYPE_U2, /* ushort */
   MTYPE_U4, /* uint */
   MTYPE_U8, /* ulong */
#ifdef longlong
   MTYPE_I8, /* long long */
   MTYPE_U8, /* unsignedlong long */
#endif
   MTYPE_U8 /* pointer */
  },
  /* ILP64 */
  {MTYPE_I2, /* short */
   MTYPE_I8, /* int */
   MTYPE_I8, /* long */
   MTYPE_U2, /* ushort */
   MTYPE_U8, /* uint */
   MTYPE_U8, /* ulong */
#ifdef longlong
   MTYPE_I8, /* long long */
   MTYPE_U8, /* unsignedlong long */
#endif
   MTYPE_U8 /* pointer */
  }
};

#define CIM_short(x)	(C_Int_Model[x].mtype_short)
#define CIM_int(x)	(C_Int_Model[x].mtype_int)
#define CIM_long(x)	(C_Int_Model[x].mtype_long)
#define CIM_ushort(x)	(C_Int_Model[x].mtype_ushort)
#define CIM_uint(x)	(C_Int_Model[x].mtype_uint)
#define CIM_ulong(x)	(C_Int_Model[x].mtype_ulong)
#define CIM_pointer(x)	(C_Int_Model[x].mtype_pointer)
#ifdef longlong
#define CIM_long_long(x) (C_Int_Model[x].mtype_long_long)
#define CIM_ulong_long(x) (C_Int_Model[x].mtype_ulong_long)
#endif
#endif /* if defined(CFE) || defined(FFE) */

void Initialize_C_Int_Model( void ) {
#ifdef TARG_MIPS 
  if (TARGET_64BIT)
    Target_Int_Model = TARGET_INT_LP64;
  //  if (TARGET_BIG_ENDIAN)
  //    Target_Byte_Sex = BIG_ENDIAN;
  //  else Target_Byte_Sex = LITTLE_ENDIAN;
  Target_Byte_Sex = LITTLE_ENDIAN;
#endif
}
   
#if defined(CFE) || defined(FFE)

TYPE_ID FE_Int_Type_To_Mtype(int t)
{
}

TYPE_ID FE_Pointer_Type_To_Mtype( void ) 
{
  return CIM_pointer(Target_Int_Model);
}

TYPE_ID FE_int_To_Mtype( void ) 
{
  return CIM_int(Target_Int_Model);
}


void Make_Int_Model_Consistent( void )
{
   Use_32_Bit_Pointers =
     MTYPE_size_reg(CIM_pointer(Target_Int_Model)) == 32;
}

#endif /* if defined(CFE) || defined(FFE) */

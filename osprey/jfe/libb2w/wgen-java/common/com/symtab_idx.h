/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#ifndef symtab_idx_INCLUDED
#define symtab_idx_INCLUDED

#ifndef ERRORS_INCLUDED
#include "errors.h"			// for Is_True assertion checking
#endif

// indices to various tables

typedef mUINT32 ST_IDX;			// idx to symbol table
typedef mUINT32 TY_IDX;			// idx to type table
typedef mUINT32 FLD_IDX;		// idx to struct field table
typedef mUINT32 ARB_IDX;		// idx to array bound table
typedef mUINT32 TYLIST_IDX;		// idx to function parm. list table
typedef mUINT32 PU_IDX;			// idx to pu table
typedef mUINT32 TARGET_INFO_IDX;	// idx to target_info table
typedef mUINT32 TCON_IDX;		// idx to tcon table
typedef mUINT32 LABEL_IDX;		// idx to label table
typedef mUINT32 PREG_IDX;		// idx to preg table
typedef mUINT32 ST_ATTR_IDX;		// idx to st_attr table
typedef mUINT32 BLK_IDX;		// idx to block table
typedef mUINT32 INITO_IDX;		// idx to initialized object table
typedef mUINT32 INITV_IDX;		// idx to initial value table
typedef mUINT64 STR_IDX;		// idx to string table
typedef mUINT8  SYMTAB_IDX;		// idx to scope array

// Reserved values for each index type.

static const ST_IDX ST_IDX_ZERO = 0;
static const TY_IDX TY_IDX_ZERO = 0;
static const FLD_IDX FLD_IDX_ZERO = 0;
static const ARB_IDX ARB_IDX_ZERO = 0;
static const TYLIST_IDX TYLIST_IDX_ZERO = 0;
static const PU_IDX PU_IDX_ZERO = 0;
static const TARGET_INFO_IDX TARGET_INFO_IDX_ZERO = 0;
static const TCON_IDX TCON_IDX_ZERO = 0;
static const LABEL_IDX LABEL_IDX_ZERO = 0;
static const PREG_IDX PREG_IDX_ZERO = 0;
static const ST_ATTR_IDX ST_ATTR_IDX_ZERO = 0;
static const BLK_IDX BLK_IDX_ZERO = 0;
static const INITO_IDX INITO_IDX_ZERO = 0;
static const INITV_IDX INITV_IDX_ZERO = 0;
static const STR_IDX STR_IDX_ZERO = 0;
static const SYMTAB_IDX SYMTAB_IDX_ZERO = 0;


// forward declarations
struct ST;
struct TY;
struct FLD;
struct ARB;
struct PU;
struct TCON;
struct LABEL;
struct PREG;
struct ST_ATTR;
struct INITO;
struct INITV;
struct SCOPE;
struct FILE_INFO;

// index to a symbol table entry
//
// It should really be:
//
// struct ST_IDX {
//     mUINT32 index : 24;
//     mUINT32 level : 8;
// };
//
// However, until we generate good code for passing small struct as function
// parameter, we need to keep it as an INT for efficiency.

template<class INDEX_TYPE, class LEVEL_INDEX_TYPE>
inline LEVEL_INDEX_TYPE Extract_level8(INDEX_TYPE idx, LEVEL_INDEX_TYPE *)
{
  return idx & 0xff;
}

template<class INDEX_TYPE>
inline UINT32 Extract_index24(INDEX_TYPE idx)          { return idx >> 8; }

inline SYMTAB_IDX
ST_IDX_level (ST_IDX st_idx)
{
  return Extract_level8(st_idx, (SYMTAB_IDX *) 0);
}

inline UINT32
ST_IDX_index (ST_IDX st_idx)	{ return Extract_index24(st_idx); }

inline ST_IDX
make_ST_IDX (UINT index, SYMTAB_IDX level) {
    return (ST_IDX)((index << 8)|level);
}

// similar to ST_IDX, INITO_IDX consists of a level and index
inline SYMTAB_IDX
INITO_IDX_level (INITO_IDX inito_idx)
{
  return Extract_level8(inito_idx, (SYMTAB_IDX *) 0);
}

inline UINT32
INITO_IDX_index (INITO_IDX inito_idx)	{ return Extract_index24(inito_idx); }

inline INITO_IDX
make_INITO_IDX (UINT index, SYMTAB_IDX level) {
    return (INITO_IDX) ((index << 8)|level);
}

template<class ENTRY_TYPE,
         class INDEX_TYPE,
         class LEVEL_INDEX_TYPE,
         class LEVEL_CONTAINER_TYPE,
         LEVEL_CONTAINER_TYPE *level_container_ptr,
         class LEVEL_MEMBER_ACCESSOR>
struct TABLE_INDEXED_BY_LEVEL8_AND_INDEX24 {
  ENTRY_TYPE &operator[](INDEX_TYPE idx)
    {
      LEVEL_INDEX_TYPE level = Extract_level8(idx, (LEVEL_INDEX_TYPE *) 0);
      UINT32           index = Extract_index24(idx);
      return LEVEL_MEMBER_ACCESSOR()(level_container_ptr, level)->Entry(index);
    }

  ENTRY_TYPE &operator()(LEVEL_CONTAINER_TYPE *lev_container_ptr,
			 INDEX_TYPE idx)
    {	// explicit version
      LEVEL_INDEX_TYPE level = Extract_level8(idx, (LEVEL_INDEX_TYPE *) 0);
      UINT32           index = Extract_index24(idx);
      return LEVEL_MEMBER_ACCESSOR()(lev_container_ptr, level)->Entry(index);
    }

  ENTRY_TYPE &operator()(LEVEL_INDEX_TYPE level, UINT32 index)
    {
#ifndef __GNU_BUG_WORKAROUND
      Is_True(LEVEL_MEMBER_ACCESSOR()(level_container_ptr, level) != NULL,
	      ("Uninitialized ST_IDX-indexed table"));
#endif
      return LEVEL_MEMBER_ACCESSOR()(level_container_ptr, level)->Entry(index);
    }

  ENTRY_TYPE &operator()(LEVEL_CONTAINER_TYPE *lev_container_ptr,
			 LEVEL_INDEX_TYPE level, UINT32 index)
    {	// explicit version
#ifndef __GNU_BUG_WORKAROUND
      Is_True(LEVEL_MEMBER_ACCESSOR()(lev_container_ptr, level) != NULL,
	      ("Uninitialized ST_IDX-indexed table"));
#endif
      return LEVEL_MEMBER_ACCESSOR()(lev_container_ptr, level)->Entry(index);
    }
};

// index to a type table entry
//
// Same reason as in ST_IDX, we define TY_IDX as an INT instead of a small
// struct.
//
// Logically, it should be:
//
// struct TY_IDX {
//     UINT index : 24;
//     UINT _restrict : 1;
//     UINT _volatile : 1;
//     UINT _const : 1;
//     UINT _align : 5;
// };

const UINT32 TY_RESTRICT	= 0x00000080; // restrict type qualifier
const UINT32 TY_VOLATILE	= 0x00000040; // volatile type qualifier
const UINT32 TY_CONST		= 0x00000020; // const type qualifier
const UINT32 TY_ALIGN		= 0x0000001f; // bit mask for alignment field

inline UINT32 TY_IDX_index (TY_IDX ty_idx)	{ return ty_idx >> 8; }
inline void Set_TY_IDX_index (TY_IDX &ty_idx, UINT32 index)
{
    ty_idx = (ty_idx & 0xff) | (index << 8);
}

inline TY_IDX
make_TY_IDX (UINT32 index)	{ return index << 8; }

inline BOOL TY_is_const (TY_IDX ty_idx)          { return ty_idx & TY_CONST; }
inline void Set_TY_is_const (TY_IDX &ty_idx)     { ty_idx |= TY_CONST; }
inline void Clear_TY_is_const (TY_IDX &ty_idx)   { ty_idx &= ~TY_CONST; }

inline BOOL TY_is_volatile (TY_IDX ty_idx)	 { return ty_idx & TY_VOLATILE;}
inline void Set_TY_is_volatile (TY_IDX &ty_idx)	 { ty_idx |= TY_VOLATILE; }
inline void Clear_TY_is_volatile (TY_IDX &ty_idx){ ty_idx &= ~TY_VOLATILE; }

inline BOOL TY_is_restrict (TY_IDX ty_idx)       { return ty_idx & TY_RESTRICT;}
inline void Set_TY_is_restrict (TY_IDX &ty_idx)  { ty_idx |= TY_RESTRICT; }
inline void Clear_TY_is_restrict (TY_IDX &ty_idx){ ty_idx &= ~TY_RESTRICT; }

// TY_align_exp returns the exponent part of the alignment.  That is, the
// true alignment in bytes is 2 to the power of TY_align_exp ()
inline UINT TY_align_exp (TY_IDX ty_idx)	{ return ty_idx & TY_ALIGN; }
inline void Set_TY_align_exp (TY_IDX &ty_idx, UINT exp)	{
    ty_idx = (ty_idx & ~TY_ALIGN) | (exp & TY_ALIGN);
}

inline UINT TY_align (TY_IDX ty_idx)	{ return 1 << (ty_idx & TY_ALIGN); }
inline void Set_TY_align (TY_IDX &ty_idx, UINT32 align) {
    extern UINT32 TY_log_base2 (UINT32 align);
    
    if (align & 0x3f)
	Set_TY_align_exp (ty_idx,
			  (align & 0x7) ? (align >> 1) : 3 + (align >> 4));
    else
	Set_TY_align_exp (ty_idx, TY_log_base2 (align));
}


#endif /* symtab_idx_INCLUDED */



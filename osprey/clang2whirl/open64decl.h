/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

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

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#ifndef OPEN64DECL_H
#define OPEN64DECL_H

// forward declaration for open64 classes
typedef struct mem_pool MEM_POOL;
typedef struct pu_info PU_Info;
typedef struct DST_idx DST_INFO_IDX;

class WN;

class ST;

class TY;

class FLD_HANDLE;

// forward declaration for open64 type definitions
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef unsigned int FLD_IDX;
typedef unsigned int LABEL_IDX;
typedef unsigned int ST_IDX;
typedef unsigned long long STR_IDX;
typedef unsigned char SYMTAB_IDX;
typedef unsigned int TCON_IDX;
typedef unsigned int TY_IDX;
typedef unsigned int TYPE_ID;
typedef unsigned int INITV_IDX;

#endif /* OPEN64DECL_H */

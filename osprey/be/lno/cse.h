/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2008 PathScale, LLC.  All Rights Reserved.
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


// -*-C++-*-

/**
*** Cse inter-iteration additions
***
*** Exported Types
***
*** Exported Functions:
***
***    extern void Inter_Iteration_Cses(WN *func_nd)
***
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef wn_INCLUDED
#include "wn.h"
#endif
#include "cxx_hash.h"

#ifndef cse_declare

#define cse_declare

class WN;
typedef struct mem_pool MEM_POOL;

class EQUIV_PAIR {
public:
  mUINT16 From;
  mUINT16 To;
  EQUIV_PAIR (mUINT16 f, mUINT16 t) { From = f; To = t;}
};

class WN_PAIR {
public:
  WN *Wn1;
  WN *Wn2;
  WN_PAIR (WN *wn1, WN *wn2) { Wn1 = wn1; Wn2 = wn2; };
};

class WN_PAIR_EC {
public:
  WN *Wn1;
  WN *Wn2;
  INT32 EClass;
  WN_PAIR_EC (WN *wn1, WN *wn2, INT32 eclass) { 
   Wn1 = wn1; Wn2 = wn2; EClass = eclass;
  };
};




typedef HASH_TABLE<UINT32,WN_PAIR*> EQUIV_WN_HASH_TABLE;
class EQUIV_WN_HASH {
  EQUIV_WN_HASH_TABLE _table;
  // intermix From and Two
  UINT32 Signature(EQUIV_PAIR ep) {
    UINT32 result1 = (ep.From & 0xF0F0);
    UINT32 result2 = ((ep.From & 0x0F0F) << 16);
    UINT32 result3 = (ep.To & 0x0F0F);
    UINT32 result4 = ((ep.To & 0xF0F0) << 16);
    UINT32 result =  result1 | result2 | result3 | result4 ;
    return result;
  }
public:
  EQUIV_WN_HASH(const UINT elements,MEM_POOL *pool) : _table(elements,pool) {;}
  void Remove(EQUIV_PAIR ep) { _table.Remove(Signature(ep)); }
  WN_PAIR *Find(EQUIV_PAIR ep) { return _table.Find(Signature(ep)); }
  void Enter(EQUIV_PAIR ep, WN_PAIR *wp) { _table.Enter(Signature(ep),wp); }
};

extern void Inter_Iteration_Cses(WN *func_nd);
#ifdef KEY
extern BOOL Invariant_Factorization(WN *func_nd);
extern BOOL Is_Invariant_Factorization_Beneficial(WN *loop);
#endif
#ifdef _KEEP_RCS_ID
static char *cse_rcs_id = cse_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */


#endif






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



#ifndef FIZ_FUSE_RCS_ID
#define FIZ_FUSE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *fiz_fuse_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */
#endif

#ifndef _fiz_fuse_INCLUDED
#define _fiz_fuse_INCLUDED

#include "defs.h"
#include "cxx_template.h"
#include "wn.h"
#include "lnopt_main.h"
#include "lnoutils.h"
#include "ff_utils.h"

typedef enum {
  Invalid=0,
  Non_SNL=1,
  Not_Inner=2,
  Inner=3
} SNL_TYPE;

class SNL_INFO {
  INT8		_depth;
  WN*		_wn;
  SNL_TYPE	_type;
public:
  friend class FIZ_FUSE_INFO;
  void Init(void) { _depth=0; _wn=NULL; _type=Invalid; };
  SNL_INFO() { Init(); }
  ~SNL_INFO(){};
  SNL_INFO(WN* wn);
};

class FIZ_FUSE_INFO {
  DYN_ARRAY<SNL_INFO> _snl_info;
  MEM_POOL            *_mpool;
public:

  FIZ_FUSE_INFO& operator += (const FIZ_FUSE_INFO& in_info) {
    INT i,j;
    for (i=0; i<=in_info._snl_info.Lastidx(); i++) {
      j = _snl_info.Newidx();
      _snl_info[j]._depth=in_info._snl_info[i]._depth;
      _snl_info[j]._wn=in_info._snl_info[i]._wn;
      _snl_info[j]._type=in_info._snl_info[i]._type;
    }

    return *this;
  }

  FIZ_FUSE_INFO(MEM_POOL* pool){
    _snl_info.Set_Mem_Pool(pool);
    _mpool = pool;
  };
  ~FIZ_FUSE_INFO(){ _snl_info.Free_array(); };
  INT8 Get_Depth(INT i)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Get_Depth() out of bound.\n"));
    return _snl_info[i]._depth;
  }
  void Set_Depth(INT i, INT8 depth)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Set_Depth() out of bound.\n"));
    _snl_info[i]._depth=depth;
  }
  WN* Get_Wn(INT i)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Get_Wn() out of bound.\n"));
    return _snl_info[i]._wn;
  }
  void Set_Wn(INT i, WN* wn)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Set_Depth() out of bound.\n"));
    _snl_info[i]._wn=wn;
  }
  SNL_TYPE Get_Type(INT i)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Get_Wn() out of bound.\n"));
    return _snl_info[i]._type;
  }
  void Set_Type(INT i, SNL_TYPE type)	{
    Is_True(i<=_snl_info.Lastidx(), ("Index to Set_Depth() out of bound.\n"));
    _snl_info[i]._type=type;
  }
  INT Num_Snl() { return _snl_info.Lastidx()+1; }
  INT New_Snl(WN* wn, INT8 depth, SNL_TYPE type) {
    INT i = _snl_info.Newidx();
    _snl_info[i]._wn = wn;
    _snl_info[i]._depth = depth;
    _snl_info[i]._type = type;
    return i;
  }
  INT New_Snl(SNL_INFO& snl_info) {
    return New_Snl(snl_info._wn, snl_info._depth, snl_info._type);
  }
  INT Copy_Snl(FIZ_FUSE_INFO *info, INT id) {
    INT i = _snl_info.Newidx();
    _snl_info[i]._wn = info->_snl_info[id]._wn;
    _snl_info[i]._depth = info->_snl_info[id]._depth;
    _snl_info[i]._type = info->_snl_info[id]._type;
    return i;
  }
  void Delete_Last_Snl() {
    _snl_info.Decidx();
  }
  void Print(FILE *fp=stdout) {
    fprintf(fp,"Print FIZ_FUSE_INFO:\n");
    for (INT i=0; i<=_snl_info.Lastidx(); i++) {
      Print(i, fp);
    }
  }
  void Print(INT i, FILE* fp=stdout);
  void Check();
  void Build(WN* func_nd, BOOL all_loops=FALSE);
};

extern FISSION_FUSION_STATUS 
  Fuse_Level_By_Level(WN* loop1, WN* loop2, UINT* fusion_level,
                      UINT peeling_limit, BOOL allow_partial_fusion=FALSE,
                      BOOL allow_outer_peeling=FALSE, FIZ_FUSE_INFO* ffi=NULL);

extern FIZ_FUSE_INFO* Fiz_Fuse(WN* loop, FIZ_FUSE_INFO* snls, MEM_POOL *mpool);

extern FIZ_FUSE_INFO*
  If_While_Region_Fiz_Fuse(WN* wn, FIZ_FUSE_INFO* snls, MEM_POOL* mpool);

#endif


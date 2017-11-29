/*
  Copyright (C) 2006-2007, Hewlett-Packard Company & Tsinghua University.  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef opt_wovp_INCLUDED
#define opt_wovp_INCLUDED "opt_wovp.h"

#include "opt_bb.h"           //BB_LIST_CONTAINER
#include "cxx_memory.h"       //CXX_NEW* && MEM_POOL
#include "opt_cfg.h"          //CFG
#include "opt_sym.h"          //OPT_STAB
#include "opt_htable.h"       //CODEREP && STMTREP
#include "errors.h"            //Is_True DevWarn
#include "defs.h"             //IDTYPE
#include "vector"             //Vector

using namespace std;

class WO_LOC{
private:
  IDTYPE    _lhs_aux_id;
  IDTYPE    _rhs_aux_id;
  STMTREP*  _stmtrep;
  BOOL      _promote;
public:
  WO_LOC(IDTYPE l, IDTYPE r, STMTREP* st){
    _lhs_aux_id = l;
    _rhs_aux_id = r;
    _stmtrep    = st;
    _promote    = FALSE;
  }
  ~WO_LOC(){}
  IDTYPE&  Get_lid()           {return _lhs_aux_id;}
  IDTYPE&  Get_rid()           {return _rhs_aux_id;}
  STMTREP* Get_stmt()          {return _stmtrep;}
  BOOL     Get_promote()       {return _promote;}
  void     Set_promote(BOOL p) {_promote = p;}
};

class WOVP{
private:
  vector<WO_LOC*> _wovp_loc;
  MEM_POOL  _pool;
  CFG      *_cfg;
  OPT_STAB *_opt_stab;

  void Add_wo_loc(IDTYPE l, IDTYPE r, STMTREP* stmt)
  {
    Is_True(stmt!=NULL, ("StmtREP is NULL"));
    _wovp_loc.push_back(CXX_NEW(WO_LOC(l, r, stmt), &_pool));
  }
  CODEREP*  Find_by_id(CODEREP *cr, IDTYPE id);
  void      Print_wo_loc(FILE *fp=stderr);
  BOOL      Write_once_check(IDTYPE id, BB_LIST_CONTAINER *bb_queue);
  void      Canon_cr();

public:
  WOVP(CFG *cfg, OPT_STAB *opt_stab)
  {
    _cfg = cfg;
    _opt_stab = opt_stab;
    MEM_POOL_Initialize(&_pool, "WOVP_pool", FALSE);
    MEM_POOL_Push(&_pool);
  }
  ~WOVP()
  {
    MEM_POOL_Pop(&_pool);
    MEM_POOL_Delete(&_pool);
  }

  void Find_mm_pair();
  BOOL Is_write_once(IDTYPE id);
  void Promote(void);
  void Do_wovp();
};
#endif  // opt_wovp_INCLUDE

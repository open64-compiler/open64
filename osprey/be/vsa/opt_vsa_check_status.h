/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ====================================================================
// ATTENTION:
//   This enum defination was move from opt_vsa.h
//   Because opt_vsa_vsym_tracker.h need this enum, so put them into
// a standlone file, once we move opt_vsa_vsym_tracker to opt_vsa_checerk.h
// , the content of this file should be moved to opt_vsa.h
//
// ====================================================================

#ifndef OPT_VSA_CHECK_STATUS_H
#define OPT_VSA_CHECK_STATUS_H

#include "opt_addr_util.h"

enum CHECKER_STATUS {
  CS_DONE,           // done this check
  CS_OP,             // check the coderep OP
  CS_VAR_UD,         // continue with the VAR U-D chain
  CS_IVAR_UD,        // continue with the IVAR U-D chain
  CS_VSYM_UD,        // continue with the VSYM U-D chain
  CS_CONT,           // continue with original check
};

enum CHECK_KIND {
  CHECK_BY_NONE,
  CHECK_BY_VAR,
  CHECK_BY_VSYM,
};

// =============================================================================
//
// CHECK_OBJ: store the current check object:
//  [1]: CHECK_BY_NONE: invalid object
//  [2]: CHECK_BY_VAR:  current check object is coderep, _vor is always NULL
//  [3]: CHECK_BY_VSYM: current check object is vsym, _cr is the coderep
//                      binded to the vor
//
// =============================================================================
class CHECK_OBJ {
private:
  CHECK_KIND          _kind;    // current check kind
  CODEREP            *_cr;      // current coderep being checked
  VSYM_OBJ_REP       *_vor;     // current vsym being checked
  STMTREP            *_sr;      // current statemet being checked
  PHI_NODE           *_phi;     // current phi being checked, _sr is NULL when _phi is set
  BB_NODE            *_bb;      // bb where inital check started, for value range check
                                // adjust at function boundary/Phi
  VSA_ACCESS_INFO     _access_info;          // iload/istore access info

  CHECK_OBJ& operator = (const CHECK_OBJ&);  // REQUIRED UNDEFINED UNWANTED methods

public:
  CHECK_OBJ(void) :
    _kind(CHECK_BY_NONE), _cr(NULL), _vor(NULL), _sr(NULL), _phi(NULL),
    _bb(NULL), _access_info()      {}
  CHECK_OBJ(STMTREP *sr) :
    _kind(CHECK_BY_NONE), _cr(NULL), _vor(NULL), _sr(sr), _phi(NULL),
    _bb(sr->Bb()), _access_info()  {}
  CHECK_OBJ(CODEREP *cr, PHI_NODE *phi, BB_NODE *pred) :
    _kind(CHECK_BY_VAR), _cr(cr), _vor(NULL), _sr(NULL), _phi(phi),
    _bb(pred), _access_info()      {}
  CHECK_OBJ(CODEREP *cr, STMTREP *sr) :
    _kind(CHECK_BY_VAR), _cr(cr), _vor(NULL), _sr(sr), _phi(NULL),
    _bb(sr->Bb()), _access_info()  {}
  CHECK_OBJ(VSYM_OBJ_REP *vor, PHI_NODE *phi, BB_NODE *pred) :
     _kind(CHECK_BY_VSYM), _cr(NULL), _vor(vor), _sr(NULL), _phi(phi),
     _bb(pred), _access_info()     {}
  CHECK_OBJ(VSYM_OBJ_REP *vor, STMTREP *sr) :
     _kind(CHECK_BY_VSYM), _cr(NULL), _vor(vor), _sr(sr), _phi(NULL),
     _bb(sr->Bb()), _access_info() {}
  CHECK_OBJ(const CHECK_OBJ &o) :
     _kind(o._kind), _cr(o._cr), _vor(o._vor), _sr(o._sr), _phi(o._phi),
     _bb(o._bb), _access_info()    {}

  CODEREP       *Coderep(void) const            {
                                                  Is_True(Is_var() && _cr, ("CHECK_OBJ not var"));
                                                  return _cr;
                                                }
  VSYM_OBJ_REP  *Vor(void) const                {
                                                  Is_True(Is_vsym() && _vor, ("CHECK_OBJ not vsym"));
                                                  return _vor;
                                                }
  CODEREP       *Vor_cr(void) const             {
                                                  Is_True(Is_vsym(), ("CHECK_OBJ not vsm"));
                                                  return _cr;
                                                }

  CHECK_KIND     Kind(void) const               { return _kind; }
  STMTREP       *Stmtrep(void) const            { return _sr;   }
  PHI_NODE      *Phi(void) const                { return _phi;  }
  BB_NODE       *Bb(void) const                 { Is_True(_bb, ("bb is null")); return _bb; }
  BOOL           Is_var(void) const             { return (_kind == CHECK_BY_VAR);  }
  BOOL           Is_vsym(void) const            { return (_kind == CHECK_BY_VSYM); }
  BOOL           Is_valid(void) const           { return ((Is_var() && _cr) || (Is_vsym() && _vor)); }

  VSA_ACCESS_INFO &Access_info()                { return _access_info;   }

  void           Set_stmtrep(STMTREP* sr)       { _sr = sr; _phi = NULL; }
  void           Set_phi(PHI_NODE *phi,
                         BB_NODE *pred)         { _sr = NULL, _phi = phi, _bb = pred; }

  void           Update_var(CODEREP *cr)        { _kind = CHECK_BY_VAR; _cr = cr; _vor = NULL; }
  void           Update_var(CODEREP *cr,
                            STMTREP *sr)        {
                                                  _kind = CHECK_BY_VAR;
                                                  _cr = cr; _vor = NULL; _sr = sr; _phi = NULL;
                                                }

  void           Update_vsym(VSYM_OBJ_REP *vor) { _kind = CHECK_BY_VSYM; _vor = vor; _cr = NULL; }
  void           Update_vsym(VSYM_OBJ_REP *vor,
                             STMTREP *sr)       {
                                                  _kind = CHECK_BY_VSYM;
                                                  _vor = vor; _cr = NULL; _sr = sr; _phi = NULL;
                                                }
  void           Update_vsym(VSYM_OBJ_REP *vor,
                             STMTREP *sr,
                             CODEREP *cr)       {
                                                  _kind = CHECK_BY_VSYM;
                                                  _vor = vor; _cr = cr; _sr = sr; _phi = NULL;
                                                }

};
#endif

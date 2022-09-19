//-*-c++-*-

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
// ====================================================================
//
// Module: opt_vsa_ssa.h
//
// ====================================================================
//

#ifndef opt_vsa_ssa_INCLUDED
#define opt_vsa_ssa_INCLUDED

// extention to opt_ssa.h for VSA

#include "opt_ssa.h"

enum EXT_PHI_FLAG {
  // check PHI_NODE::PHI_NODE_FLAGS to make sure no duplication
  PNF_RES_IS_HOR         = 0x1000, // res and opnd are HEAP_OBJ_REP
  PNF_RES_IS_VOR         = 0x2000, // res and opnd are VSYM_OBJ_REP
  PNF_OPND_MISMATCH      = 0x4000, // res and opnd are different object
};

class VSA_PHI_NODE {
private:
  PHI_NODE *_phi;

  VSA_PHI_NODE();
  VSA_PHI_NODE(const VSA_PHI_NODE&);
  VSA_PHI_NODE& operator=(const VSA_PHI_NODE&);

public:
  VSA_PHI_NODE(PHI_NODE *phi) : _phi(phi) { }

  BOOL Res_is_hor(void) const    { return _phi->Flags() & PNF_RES_IS_HOR; }
  void Set_res_is_hor(void)      { _phi->Set_flags(_phi->Flags() | PNF_RES_IS_HOR); }
  BOOL Res_is_vor(void) const    { return _phi->Flags() & PNF_RES_IS_VOR; }
  void Set_res_is_vor(void)      { _phi->Set_flags(_phi->Flags() | PNF_RES_IS_VOR); }
  BOOL Opnd_mismatch(void) const { return _phi->Flags() & PNF_OPND_MISMATCH; }
  void Set_opnd_mismatch(void)   { _phi->Set_flags(_phi->Flags() | PNF_OPND_MISMATCH); }
};

inline BOOL
Phi_res_is_hor(const PHI_NODE* phi)    { return phi->Flags() & PNF_RES_IS_HOR; }
inline BOOL
Phi_res_is_vor(const PHI_NODE* phi)    { return phi->Flags() & PNF_RES_IS_VOR; }
inline BOOL
Phi_opnd_mismatch(const PHI_NODE* phi) { return phi->Flags() & PNF_OPND_MISMATCH; }


#endif /* opt_vsa_ssa_INCLUDED */


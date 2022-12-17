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

// =============================================================================
// Utilities
// =============================================================================
static inline BOOL
Is_cr_opnd0_abs(CODEREP *cr) {
  return (cr->Kind() == CK_OP &&
          ((cr->Opr() == OPR_ABS &&
            cr->Opnd(0)->Kind() == CK_VAR) ||
           ((cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) &&
            cr->Opnd(0)->Kind() == CK_OP &&
            cr->Opnd(0)->Opr() == OPR_ABS &&
            (cr->Opnd(0)->Opnd(0)->Kind() == CK_VAR ||
             (cr->Opnd(0)->Opnd(0)->Kind() == CK_OP &&
              (cr->Opnd(0)->Opnd(0)->Opr() == OPR_CVT ||
               cr->Opnd(0)->Opnd(0)->Opr() == OPR_CVTL) &&
              cr->Opnd(0)->Opnd(0)->Opnd(0)->Kind() == CK_VAR)))));
}

static inline BOOL
Is_cr_const_0(CODEREP *cr) {
  return (cr->Kind() == CK_CONST &&
          cr->Const_val() == 0) ||
         (cr->Has_const_fval() &&
          cr->Const_fval() == 0.0);
}

// =============================================================================
//
// Handle EQ/NE/GT/GE/LT/LE
// 
// =============================================================================

// know x == lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know x != lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x > lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x >= lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x < lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x <= lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x == lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> yes
  return res == VA_YES ? VA_NO :
           res == VA_NO ? VA_YES : res;
}

// know x != lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x > lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x >= lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x < lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x <= lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x == lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know x != lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN?
}

// know x > lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x >= lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x < lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() + 1);
  else if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() - 1);
  else
    // TODO??: which one is better? lhs or rhs?
    rhs = New_binary_cr(OPCODE_make_op(OPR_ADD, rhs->Dtyp(), MTYPE_V), rhs, 1);
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x <= lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x == lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know x != lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know x > lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() + 1);
  else if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() - 1);
  else 
    // TODO??: which one is better? lhs or rhs?
    lhs = New_binary_cr(OPCODE_make_op(OPR_ADD, lhs->Dtyp(), MTYPE_V), lhs, 1);
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x >= lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x < lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x <= lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x == lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know x != lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know x > lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() + 1);
  else if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() - 1);
  else
    // TODO??: which one is better? lhs or rhs?
    lhs = New_binary_cr(OPCODE_make_op(OPR_ADD, lhs->Dtyp(), MTYPE_V), lhs, 1);
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x >= lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x < lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x <= lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x == lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_EQ, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know x != lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_NE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know x > lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GT, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x >= lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_GE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know x < lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LT, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() + 1);
  else if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() - 1);
  else
    // TODO: which one is better? lhs or rhs?
    rhs = New_binary_cr(OPCODE_make_op(OPR_ADD, rhs->Dtyp(), MTYPE_V), rhs, 1);
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know x <= lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_expr<OPR_LE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, BOOL zext,
                                  const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, zext, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// =============================================================================
//
// Handle abs expr's EQ/NE/GT/GE/LT/LE
//
// =============================================================================

// know abs(x) == lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know abs(x) != lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) > lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) >= lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x == rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) == lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> yes
  return res == VA_YES ? VA_NO :
           res == VA_NO ? VA_YES : res;
}

// know abs(x) != lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_EQ>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) > lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) >= lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x != rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) == lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know abs(x) != lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN?
}

// know abs(x) > lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN?
}

// know abs(x) >= lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() + 1);
  else if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() - 1);
  else
    // TODO??: which one is better? lhs or rhs?
    rhs = New_binary_cr(OPCODE_make_op(OPR_ADD, rhs->Dtyp(), MTYPE_V), rhs, 1);
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x > rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) == lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know abs(x) != lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know abs(x) > lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() + 1);
  else if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() - 1);
  else
    // TODO??: which one is better? lhs or rhs?
    lhs = New_binary_cr(OPCODE_make_op(OPR_ADD, lhs->Dtyp(), MTYPE_V), lhs, 1);
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) >= lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x >= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) == lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know abs(x) != lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know abs(x) > lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() + 1);
  else if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() - 1);
  else
    // TODO??: which one is better? lhs or rhs?
    lhs = New_binary_cr(OPCODE_make_op(OPR_ADD, lhs->Dtyp(), MTYPE_V), lhs, 1);
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) >= lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_NO;
  VRA_RESULT res = Compare_cr<OPR_GE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x < rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_LT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) == lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_EQ, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> no
  return res;
}

// know abs(x) != lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_NE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// know abs(x) > lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GT, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN?
}

// know abs(x) >= lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_GE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_POSSIBLE;
  VRA_RESULT res = Compare_cr<OPR_GT>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> no, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

// know abs(x) < lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LT, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  if (rhs->Kind() == CK_CONST)
    rhs = New_cr(rhs->Dtyp(), rhs->Const_val() + 1);
  else if (lhs->Kind() == CK_CONST)
    lhs = New_cr(lhs->Dtyp(), lhs->Const_val() - 1);
  else
    // TODO: which one is better? lhs or rhs?
    rhs = New_binary_cr(OPCODE_make_op(OPR_ADD, rhs->Dtyp(), MTYPE_V), rhs, 1);
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// know abs(x) <= lhs, check if x <= rhs
template<> VRA_RESULT
VRA::Compare_abs_expr<OPR_LE, OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (lhs == rhs) return VA_YES;
  VRA_RESULT res = Compare_cr<OPR_LE>(lhs, bb_id, rhs, FALSE, paths, visited);
  // unk -> unk, poss -> poss, yes -> yes, no -> poss
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_YES : VA_POSSIBLE;
}

// =============================================================================
//
// Handle ADD/SUB/NEG
// 
// =============================================================================


// =============================================================================
//
// Compare two constants
// 
// =============================================================================

template<> VRA_RESULT
VRA::Compare_const<OPR_EQ>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST)
    return lhs->Const_val() == rhs->Const_val() ? VA_YES : VA_NO;
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

template<> VRA_RESULT
VRA::Compare_const<OPR_NE>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST)
    return lhs->Const_val() != rhs->Const_val() ? VA_YES : VA_NO;
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

template<> VRA_RESULT
VRA::Compare_const<OPR_GT>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST) {
    if (zext)
      return (UINT64)lhs->Const_val() > (UINT64)rhs->Const_val() ? VA_YES : VA_NO;
    else
      return lhs->Const_val() > rhs->Const_val() ? VA_YES : VA_NO;
  }
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

template<> VRA_RESULT
VRA::Compare_const<OPR_GE>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST) {
    if (zext)
      return (UINT64)lhs->Const_val() >= (UINT64)rhs->Const_val() ? VA_YES : VA_NO;
    else
      return lhs->Const_val() >= rhs->Const_val() ? VA_YES : VA_NO;
  }
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

template<> VRA_RESULT
VRA::Compare_const<OPR_LT>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST) {
    if (zext)
      return (UINT64)lhs->Const_val() < (UINT64)rhs->Const_val() ? VA_YES : VA_NO;
    else
      return lhs->Const_val() < rhs->Const_val() ? VA_YES : VA_NO;
  }
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

template<> VRA_RESULT
VRA::Compare_const<OPR_LE>(CODEREP* lhs, CODEREP* rhs, bool zext) const
{
  Is_True(lhs->Kind() == CK_CONST, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST) {
    if (zext)
      return (UINT64)lhs->Const_val() <= (UINT64)rhs->Const_val() ? VA_YES : VA_NO;
    else
      return lhs->Const_val() <= rhs->Const_val() ? VA_YES : VA_NO;
  }
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
}

// =============================================================================
//
// Compare two floating point constants
//
// =============================================================================

template<> VRA_RESULT
VRA::Compare_rconst<OPR_EQ>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val == rhs_val ? VA_YES : VA_NO;
}

template<> VRA_RESULT
VRA::Compare_rconst<OPR_NE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val != rhs_val ? VA_YES : VA_NO;
}

template<> VRA_RESULT
VRA::Compare_rconst<OPR_GT>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val > rhs_val ? VA_YES : VA_NO;
}

template<> VRA_RESULT
VRA::Compare_rconst<OPR_GE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val >= rhs_val ? VA_YES : VA_NO;
}

template<> VRA_RESULT
VRA::Compare_rconst<OPR_LT>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val < rhs_val ? VA_YES : VA_NO;
}

template<> VRA_RESULT
VRA::Compare_rconst<OPR_LE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Has_const_fval(), ("Unexpected lhs kind %d", lhs->Kind()));
  double lhs_val = lhs->Const_fval();
  double rhs_val = 0;
  if (rhs->Has_const_fval())
    rhs_val = rhs->Const_fval();
  else if (rhs->Kind() == CK_CONST)
    rhs_val = (double)rhs->Const_val();
  else
    return VA_UNKNOWN;  // TODO: POSSIBLE or UNKNOWN??
  return lhs_val <= rhs_val ? VA_YES : VA_NO;
}

// =============================================================================
// Compare two variables directly without value range information
// =============================================================================

template<> VRA_RESULT
VRA::Compare_var<OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_VAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_VAR &&
      !lhs->Is_flag_set(CF_IS_ZERO_VERSION) &&
      !rhs->Is_flag_set(CF_IS_ZERO_VERSION) &&
      lhs->Aux_id() == rhs->Aux_id() &&
      lhs->Version() == rhs->Version())
    return VA_YES;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_var<OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  VRA_RESULT res = Compare_var<OPR_EQ>(lhs, bb_id, rhs);
  return res == VA_UNKNOWN ? VA_UNKNOWN :
           res == VA_YES ? VA_NO : VA_POSSIBLE;
}

template<> VRA_RESULT
VRA::Compare_var<OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_VAR, ("Unexpected lhs kind %d", lhs->Kind()));
  CODEREP* rhs_canon = NULL;
  if (Canonicalize_coderep(rhs, lhs, rhs_canon) == FALSE)
    return VA_UNKNOWN;
  if (rhs_canon->Kind() == CK_VAR && rhs_canon == lhs) {
    // x > x
    return VA_NO;
  }
  if (rhs_canon->Kind() == CK_OP) {
    CODEREP* rhs_lhs = rhs_canon->Opnd(0);
    if (rhs_lhs->Kind() == CK_OP) {
      // x > -x +/- a
      return VA_UNKNOWN;
    }
    CODEREP* rhs_rhs = rhs_canon->Opnd(1);
    if (rhs_canon->Opr() == OPR_ADD && rhs_rhs == lhs) {
      rhs_rhs = rhs_lhs;
      rhs_lhs = lhs;
    }
    Is_True(rhs_lhs == lhs,
            ("invalid canonicalization result"));
    PATH_SELECTED paths;
    switch (rhs_canon->Opr()) {
    case OPR_ADD:
      // x > x+a ==> a < 0
      return Expr_cmp_val<OPR_LT>(rhs_rhs, bb_id, (INT64)0, paths);
    case OPR_SUB:
      // x > x-a ==> a > 0
      return Expr_cmp_val<OPR_GT>(rhs_rhs, bb_id, (INT64)0, paths);
    default:
      return VA_UNKNOWN;
    }
  }
  return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_var<OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_VAR, ("Unexpected lhs kind %d", lhs->Kind()));
  CODEREP* rhs_canon = NULL;
  if (Canonicalize_coderep(rhs, lhs, rhs_canon) == FALSE)
    return VA_UNKNOWN;
  if (rhs_canon->Kind() == CK_VAR && rhs_canon == lhs) {
    // x >= x
    return VA_YES;
  }
  else if (rhs_canon->Kind() == CK_OP) {
    CODEREP* rhs_lhs = rhs_canon->Opnd(0);
    if (rhs_lhs->Kind() == CK_OP) {
      // x > -x +/- a
      return VA_UNKNOWN;
    }
    CODEREP* rhs_rhs = rhs_canon->Opnd(1);
    if (rhs_canon->Opr() == OPR_ADD && rhs_rhs == lhs) {
      rhs_rhs = rhs_lhs;
      rhs_lhs = lhs;
    }
    Is_True(rhs_lhs == lhs,
            ("invalid canonicalization result"));
    PATH_SELECTED paths;
    switch (rhs_canon->Opr()) {
    case OPR_ADD:
      // x >= x+a ==> a <= 0
      return Expr_cmp_val<OPR_LE>(rhs_rhs, bb_id, (INT64)0, paths);
    case OPR_SUB:
      // x >= x-a ==> a >= 0
      return Expr_cmp_val<OPR_GE>(rhs_rhs, bb_id, (INT64)0, paths);
    default:
      return VA_UNKNOWN;
    }
  }

  return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_var<OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_VAR, ("Unexpected lhs kind %d", lhs->Kind()));
  CODEREP* rhs_canon = NULL;
  if (Canonicalize_coderep(rhs, lhs, rhs_canon) == FALSE)
    return VA_UNKNOWN;
  if (rhs_canon->Kind() == CK_VAR && rhs_canon == lhs) {
    // x < x
    return VA_NO;
  }
  if (rhs_canon->Kind() == CK_OP) {
    CODEREP* rhs_lhs = rhs_canon->Opnd(0);
    if (rhs_lhs->Kind() == CK_OP) {
      // x > -x +/- a
      return VA_UNKNOWN;
    }
    CODEREP* rhs_rhs = rhs_canon->Opnd(1);
    if (rhs_canon->Opr() == OPR_ADD && rhs_rhs == lhs) {
      rhs_rhs = rhs_lhs;
      rhs_lhs = lhs;
    }
    Is_True(rhs_lhs == lhs,
            ("invalid canonicalization result"));
    PATH_SELECTED paths;
    switch (rhs_canon->Opr()) {
    case OPR_ADD:
      // x < x+a ==> a > 0
      return Expr_cmp_val<OPR_GT>(rhs_rhs, bb_id, (INT64)0, paths);
    case OPR_SUB:
      // x < x-a ==> a < 0
      return Expr_cmp_val<OPR_LT>(rhs_rhs, bb_id, (INT64)0, paths);
    default:
      return VA_UNKNOWN;
    }
  }
  return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_var<OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_VAR, ("Unexpected lhs kind %d", lhs->Kind()));
  CODEREP* rhs_canon = NULL;
  if (Canonicalize_coderep(rhs, lhs, rhs_canon) == FALSE)
    return VA_UNKNOWN;
  if (rhs_canon->Kind() == CK_VAR && rhs_canon == lhs) {
    // x <= x
    return VA_YES;
  }
  if (rhs_canon->Kind() == CK_OP) {
    CODEREP* rhs_lhs = rhs_canon->Opnd(0);
    if (rhs_lhs->Kind() == CK_OP) {
      // x > -x +/- a
      return VA_UNKNOWN;
    }
    CODEREP* rhs_rhs = rhs_canon->Opnd(1);
    if (rhs_canon->Opr() == OPR_ADD && rhs_rhs == lhs) {
      rhs_rhs = rhs_lhs;
      rhs_lhs = lhs;
    }
    Is_True(rhs_lhs == lhs,
            ("invalid canonicalization result"));
    PATH_SELECTED paths;
    switch (rhs_canon->Opr()) {
    case OPR_ADD:
      // x <= x+a ==> a >= 0
      return Expr_cmp_val<OPR_GE>(rhs_rhs, bb_id, (INT64)0, paths);
    case OPR_SUB:
      // x <= x-a ==> a <= 0
      return Expr_cmp_val<OPR_LE>(rhs_rhs, bb_id, (INT64)0, paths);
    default:
      return VA_UNKNOWN;
    }
  }
  return VA_UNKNOWN;
}

// =============================================================================
// Compare two IVARs directly without value range information
// =============================================================================

template<> VRA_RESULT
VRA::Compare_ivar<OPR_EQ>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_YES;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_ivar<OPR_NE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_ivar<OPR_GT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_ivar<OPR_GE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_YES;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_ivar<OPR_LT>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_ivar<OPR_LE>(CODEREP* lhs, UINT32 bb_id, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_IVAR, ("Unexpected lhs kind %d", lhs->Kind()));
  if (lhs == rhs)
    return VA_YES;
  else
    return VA_UNKNOWN;
}

// =============================================================================
// Compare LDA lhs with rhs without value range information
// =============================================================================

template<> VRA_RESULT
VRA::Compare_lda<OPR_EQ>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST)
    return VA_NO;   // LDA not equal to const
  else if (rhs->Kind() == CK_LDA)
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() == rhs->Offset()) ?
               VA_YES : VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_lda<OPR_NE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_CONST)
    return VA_YES;  // LDA not equal to const
  else if (rhs->Kind() == CK_LDA)
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() == rhs->Offset()) ?
               VA_NO : VA_YES;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_lda<OPR_GT>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_LDA)
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() > rhs->Offset()) ?
             VA_YES : VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_lda<OPR_GE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_LDA)
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() >= rhs->Offset()) ?
             VA_YES : VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_lda<OPR_LT>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_LDA) 
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() < rhs->Offset()) ?
             VA_YES : VA_NO;
  else
    return VA_UNKNOWN;
}

template<> VRA_RESULT
VRA::Compare_lda<OPR_LE>(CODEREP* lhs, CODEREP* rhs) const
{
  Is_True(lhs->Kind() == CK_LDA, ("Unexpected lhs kind %d", lhs->Kind()));
  if (rhs->Kind() == CK_LDA)
    return (lhs->Lda_aux_id() == rhs->Lda_aux_id() && lhs->Offset() <= rhs->Offset()) ?
             VA_YES : VA_NO;
  else
    return VA_UNKNOWN;
}


// =============================================================================
// Compare vra expr with val
// =============================================================================

template<UINT32 cmp> VRA_RESULT
VRA::Compare_vra_expr(UINT32 expr_id, UINT32 bb_id, CODEREP* val, BOOL zext,
                      const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  if (visited[expr_id] >= VISIT_MAX_COUNT)
    return VA_IGNORE;
  ++ visited[expr_id];

  const VRA_EXPR& expr = Expr(expr_id);
  CODEREP* cr;
  VRA_RESULT res = VA_UNKNOWN, res1 = VA_UNKNOWN;
  switch (expr.Opr()) {
  case OPR_TOP:
    // TODO: integration with DNA/RNA. set result to POSSIBLE for TOP
    res = VA_IGNORE;
    break;
  case OPR_BOTTOM:
  case OPR_PLACEHOLDER:
    // TODO: integration with DNA/RNA. set result to UNKNOWN for BOTTOM
    break;
  case OPR_EMPTY:
    res = VA_NO;
    break;

  case OPR_CODEREP:
    // compare with coderep
    cr = expr.Cr();
    if (cr->Kind() == CK_OP && cr->Opr() == OPR_CALL)
      return VA_POSSIBLE;
    res = Compare_cr<cmp>(cr, bb_id, val, zext, paths, visited);
    break;
  case OPR_CONJUNCTION:
    // compare with opnd(0)
    res = Compare_vra_expr<cmp>(expr.Op0(), bb_id, val, zext, paths, visited);
    if (res == VA_NO) {
      // if already get a precise result, return it
      // TODO: reconsider this more
      return res;
    }
    // compare with opnd(1)
    res1 = Compare_vra_expr<cmp>(expr.Op1(), bb_id, val, zext, paths, visited);
    if (res1 == VA_NO) {
      // if already get a precise result, return it
      return VA_NO;
    }
    if (res == VA_IGNORE && res1 == VA_IGNORE)
      return VA_UNKNOWN;
    if (res == VA_IGNORE)
      return res1;
    if (res1 == VA_IGNORE)
      return res;
    //if (res == VA_YES || res1 == VA_YES)
    //  return VA_YES;
    if (res == VA_UNKNOWN || res1 == VA_UNKNOWN)
      return VA_UNKNOWN;
    if (res1 > res) {
      // promote from UNKNOWN -> POSSIBLE -> YES -> NO
      res = res1;
    }
    break;
  case OPR_DISJUNCTION:
    {
      BOOL kid0_visited = FALSE, kid1_visited = FALSE;
      const VRA_EXPR& kid0 = Expr(expr.Op0());
      if (!paths.Selected(Dna()->Dna_idx(), expr.Bb_id()) ||
          kid0.Opr() == OPR_DISJUNCTION ||
          paths.Selected(Dna()->Dna_idx(), expr.Bb_id(), kid0.Bb_id()))
      {
        // compare with opnd(0) if there is no specific path or the path is selected
        res = Compare_vra_expr<cmp>(expr.Op0(), kid0.Bb_id(), val, zext, paths, visited);
        kid0_visited = TRUE;
      }
      const VRA_EXPR& kid1 = Expr(expr.Op1());
      if (!paths.Selected(Dna()->Dna_idx(), expr.Bb_id()) ||
          kid1.Opr() == OPR_DISJUNCTION ||
          paths.Selected(Dna()->Dna_idx(), expr.Bb_id(), kid1.Bb_id()))
      {
        // compare with opnd(1) if there is no specific path or the path is selected
        res1 = Compare_vra_expr<cmp>(expr.Op1(), kid1.Bb_id(), val, zext, paths, visited);
        kid1_visited = TRUE;
      }
      if (kid0_visited == FALSE && kid1_visited == FALSE) {
        // if no kid visited, return no
        return VA_NO;
      }
      else if (kid0_visited == FALSE || res == VA_IGNORE) {
        // if kid0 not visited, return result from kid1
        return res1 == VA_IGNORE ? VA_UNKNOWN : res1;
      }
      else if (kid1_visited == FALSE || res1 == VA_IGNORE) {
        // if kid1 not visited, return result from kid0
        return res == VA_IGNORE ? VA_UNKNOWN : res;
      }
      else if (res == VA_POSSIBLE || res1 == VA_POSSIBLE) {
        return VA_POSSIBLE;
      }
      else if (res == VA_UNKNOWN || res1 == VA_UNKNOWN) {
        return VA_UNKNOWN;
      }
      else if (res == VA_YES && res1 == VA_YES) {
        return VA_YES;
      }
      else if (res == VA_NO && res1 == VA_NO) {
        return VA_NO;
      }
      return VA_POSSIBLE;
    }
    break;
  default:
    Is_True(FALSE, ("Unknown Expr operator %d", expr.Opr()));
    break;
  }
  return res;
}

// compare OP coderep with val
template<UINT32 cmp> VRA_RESULT
VRA::Compare_op_cr(CODEREP* cr, UINT32 bb_id, CODEREP* val, BOOL zext, const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  BOOL trace = Get_Trace(TP_WOPT2, VSA_VRA_DUMP_FLAG);
  Is_True(cr->Kind() == CK_OP, ("Invalid cr kind %d", cr->Kind()));

  CODEREP* opnd0 = cr->Opnd(0);
  BOOL is_opnd0_var = (opnd0->Kind() == CK_VAR ||
                       opnd0->Kind() == CK_IVAR);
  BOOL is_opnd0_abs = Is_cr_opnd0_abs(opnd0);
  BOOL is_abs_cmp_0 = (is_opnd0_abs && Is_cr_const_0(val));

  OPERATOR opr = cr->Opr();
  VRA_RESULT res = VA_UNKNOWN;
  switch (opr) {
  case OPR_EQ:
    if (is_opnd0_var)
      return Compare_expr<OPR_EQ, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_EQ, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_NE:
    if (is_opnd0_var)
      return Compare_expr<OPR_NE, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_NE, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_GT:
    if (is_opnd0_var)
      return Compare_expr<OPR_GT, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_GT, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_GE:
    if (is_opnd0_var)
      return Compare_expr<OPR_GE, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_GE, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_LT:
    if (is_opnd0_var)
      return Compare_expr<OPR_LT, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_LT, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_LE:
    if (is_opnd0_var)
      return Compare_expr<OPR_LE, cmp>(cr->Opnd(1), bb_id, val, zext, paths, visited);
    if (is_abs_cmp_0)
      return Compare_abs_expr<OPR_LE, cmp>(cr->Opnd(1), bb_id, val, paths, visited);
    break;
  case OPR_CVT:
    return Compare_cr<cmp>(opnd0, bb_id, val, zext, paths, visited);
  case OPR_ADD:
  case OPR_SUB:
    if (opnd0->Kind() == CK_VAR &&
        (cr->Opnd(1)->Kind() == CK_CONST ||
         cr->Opnd(1)->Has_const_fval())) {
      OPERATOR nopr = opr == OPR_ADD ? OPR_SUB : OPR_ADD;
      if (val->Has_const_fval()) {
        // create a temp rconst cr in current context
        CODEREP* cr = Alloc_stack_cr(0);
        val = Init_fconst_cr(cr, val->Dtyp(), val->Const_fval());
      }
      CODEREP* nval = New_cr(OPCODE_make_op(nopr, cr->Dtyp(), MTYPE_V),
                             val, cr->Opnd(1), TRUE);

      Is_Trace(trace, (TFile, "VRA::Compare_op_cr<%s> lhs:\n", OPERATOR_name((OPERATOR)cmp) + 4));
      Is_Trace_cmd(trace, Print_coderep(cr, TFile));
      Is_Trace(trace, (TFile, "\n                       rhs:\n"));
      Is_Trace_cmd(trace, Print_coderep(val, TFile));
      Is_Trace(trace, (TFile, "\n==>>               lhs:\n"));
      Is_Trace_cmd(trace, Print_coderep(opnd0, TFile));
      Is_Trace(trace, (TFile, "\n                       rhs:\n"));
      Is_Trace_cmd(trace, Print_coderep(nval, TFile));
      Is_Trace(trace, (TFile, "\n"));

      res = Compare_cr<cmp>(opnd0, bb_id, nval, zext, paths, visited);

      Is_Trace(trace, (TFile, "==>> %d\n", res));

      break;
    }
    //if (val->Kind() == CK_CONST && val->Const_val() == 0) {
    //  return Compare_cr<OPR_NE>(cr->Opnd(0), bb_id, cr->Opnd(1), paths, visited);
    //}
  case OPR_NEG:
    //Is_True(FALSE, ("TODO, handle %s", OPERATOR_name(opr) + 4));
    // TODO
  default:
    break;
  }
  return res;
}

// compare coderep cr with val
template<UINT32 cmp> VRA_RESULT
VRA::Compare_cr(CODEREP* cr, UINT32 bb_id, CODEREP* val, BOOL zext,
                const PATH_SELECTED& paths, COUNT_ARRAY& visited) const
{
  VRA_RESULT res = VA_UNKNOWN;
  UINT32 expr;
  switch (cr->Kind()) {
  case CK_LDA:
    // compare with LDA
    return Compare_lda<cmp>(cr, val);
  case CK_CONST:
    // compare with const
    return Compare_const<cmp>(cr, val, zext);
  case CK_RCONST:
    // compare with rconst
    return cr->Has_const_fval() ? Compare_rconst<cmp>(cr, val) : VA_UNKNOWN;
  case CK_VAR:
    // compare var coderep direcltly
    res = Compare_var<cmp>(cr, bb_id, val);
    if (res == VA_YES || res == VA_NO)
      return res;
    // compare their value range
    expr = Value_range_rec(cr, bb_id);
    if (expr == VR_NOT_FOUND) {
      return Dna()->Is_param(cr) == INVALID_VAR_IDX ? VA_UNKNOWN
                                                    : VA_IGNORE;
    }
    res = Compare_vra_expr<cmp>(expr, bb_id, val, zext, paths, visited);
    if (res == VA_UNKNOWN && Dna()->Is_param(cr) != INVALID_VAR_IDX)
      return VA_IGNORE;  // give a chance for xfa value range
    return res;
  case CK_IVAR:
    res = Compare_ivar<cmp>(cr, bb_id, val);
    return res;
  case CK_OP:
    // compare OP coderep with val
    return Compare_op_cr<cmp>(cr, bb_id, val, zext, paths, visited);
  default:
    Is_True(FALSE, ("Unknown cr kind %d", cr->Kind()));
    return VA_UNKNOWN;
  }
  return res;
}

template<OPERATOR opr> VRA_RESULT NOINLINE
VRA::Compare_cr_xfa(CODEREP* lhs, IDTYPE bb_id, CODEREP* rhs,
                    DNA_NODE* rhs_dna, const PATH_SELECTED& paths) const
{
  if (lhs->Kind() == CK_CONST && rhs->Kind() == CK_CONST)
    return Compare_const<opr>(lhs, rhs, FALSE);
  if (Dna() == rhs_dna)
    return VA_POSSIBLE;

  CODEREP* lhs_in_dna = lhs;
  IDTYPE   bb_in_dna = bb_id;
  DNA_NODE* def_dna = Dna();
  BOOL ret = Vsa()->Find_def_cr_in_dna(lhs, rhs_dna, paths, lhs_in_dna, def_dna, bb_in_dna);
  return ret ? def_dna->Comp_unit()->Vra()->Expr_cmp_val<opr>(lhs_in_dna,
                                                              bb_in_dna, rhs, paths)
             : VA_POSSIBLE;
}

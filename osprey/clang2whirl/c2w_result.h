/*
  Copyright (C) 2019-2020 XC5 Limited, Inc.  All Rights Reserved.

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

#ifndef CLANG2WHIRL_RESULT_H
#define CLANG2WHIRL_RESULT_H

#include "errors.h"
#include "open64decl.h"

namespace wgen {

// Result:
// Convert clang AST node into Result and assemble WHIRL TREE with Result
//

// RV - Result VALUE
// LVALUE, RVALUE and XVALUE
enum RV {
  R_UNSPEC,   // Result is unspecified, convert according to clang ast
  R_NVALUE,   // Result is not a value
  R_LVALUE,   // Result is lvalue
  R_RVALUE,   // Result is rvalue
  R_XVALUE,   // Result is xvalue
};

// RK - Result KIND
// NONE, WHIRL NODE, WHIRL ST, INT CONSTANT
enum RK {
  R_NONE,      // None
  R_WN,        // Result is WHIRL node
  R_ST,        // Result is WHIRL st
  R_TCON,      // Result is WHIRL TCON index
  R_INITV,     // Result is WHIRL INITV index
  R_INTCONST   // Result is integer constant
};

// RF - Result FLAG
enum RF {
  RF_ADDROF       = 0x001,    // Result is for &(v)
  RF_ARRAY        = 0x002,    // Result is for v[i]
  RF_ARROW        = 0x004,    // Result is for s->field
  RF_CONST_SYM    = 0x008,    // Result is constant symbol (string literal)
  RF_DEREF        = 0x010,    // Result is for *(p)
  RF_DOT          = 0x020,    // Result is for s.field
  RF_REF          = 0x040,    // Result is for reference in C++
  RF_LVALUE       = 0x080,    // Result is lvalue
  RF_RVALUE       = 0x100,    // Result is rvalue
  RF_WN_USED      = 0x200,    // WHIRL node has been used
};

// convert clang expr into Result
// expr maybe return a symbol idx, string idx or whirl node
class Result {

private:
  union {
    WN      *_wn;           // WHIRL node
    INT64    _val;          // Integer constant value, st_idx, tcon_idx or initv_idx
  } _r;
  TY_IDX     _ty;           // TY
  mUINT16    _kind : 3;     // kind (3bit). See RK above 
  mUINT16    _flag :13;     // flag (13bit). See RF above
  mUINT16    _fld;          // field (16bit, WHIRL support 14bit)

private:
  // constructor, set all fields to 0
  Result(RK kind) {
    _r._val = 0;
    _ty = 0;
    _kind = kind;
    _flag = 0;
    _fld = 0;
  }

  // constructor for WHIRL node
  Result(WN* wn, TY_IDX ty) {
    _r._wn = wn;
    _ty = ty;
    _kind = R_WN;
    _flag = 0;
    _fld = 0;
  }

  // constructor for WHIRL st_idx, tcon_idx or initv_idx with ty_idx
  Result(RK kind, IDTYPE idx, TY_IDX ty) {
    _r._val = idx;
    _kind = kind;
    _ty = ty;
    _flag = 0;
    _fld = 0;
  }
  // constructor for integer constant
  Result(INT64 val, TY_IDX ty)
  {
    _r._val = val;
    _kind = R_INTCONST;
    _ty = ty;
    _flag = 0;
    _fld = 0;
  }
  // generate ILDA for LValue with LDID of pre and field id
  WN *Generate_Ilda(WN *addr, INT64 ofst);

public:
  // create "None" Result
  static Result nwNone()                         { return Result(R_NONE);          }
  // create "WN" Result
  static Result nwNode(WN *wn, TY_IDX ty)        { return Result(wn, ty);          }
  // create "ST" Result
  static Result nwSym(ST_IDX st, TY_IDX ty)      { return Result(R_ST, st, ty);    }
  // create "TCON" Result
  static Result nwTcon(TCON_IDX tc, TY_IDX ty)   { return Result(R_TCON, tc, ty);  }
  // create "INITV" Result
  static Result nwInitV(INITV_IDX iv, TY_IDX ty) { return Result(R_INITV, iv, ty); }
  // create "INTCONST" Result
  static Result nwIntConst(INT64 v, TY_IDX ty)   { return Result(v, ty);           }

public:
  // get Result kind
  RK        Kind() const     { return (RK)_kind; }
  // get Result type
  TY_IDX    Ty() const       { return _ty;       }
  // get Result field id
  UINT32    FieldId() const  { return _fld;      }
  // get Result WHIRL node
  WN       *Node() const     { Is_True(_kind == R_WN, ("not wn")); return _r._wn;          }
  // get Result st_idx
  ST_IDX    Sym() const      { Is_True(_kind == R_ST, ("not st")); return (ST_IDX)_r._val; }
  // get Result tcon_idx
  TCON_IDX  Tcon() const     { Is_True(_kind == R_TCON, ("not tcon")); return (TCON_IDX)_r._val;    }
  // get Result initv_idx
  INITV_IDX InitV() const    { Is_True(_kind == R_INITV, ("not initv")); return (INITV_IDX)_r._val; }
  // get Result intconst value
  INT64     IntConst() const { Is_True(_kind == R_INTCONST, ("not intconst")); return _r._val;      }

  // Check if Result kind is NONE
  BOOL isNone() const     { return _kind == R_NONE;     }
  // Check if Result kind is WHIRL node
  BOOL isNode() const     { return _kind == R_WN;       }
  // Check if Result kind is st_idx
  BOOL isSym() const      { return _kind == R_ST;       }
  // Check if Result kind is tcon_idx
  BOOL isTCon() const     { return _kind == R_TCON;     }
  // Check if Result kind is initv_idx
  BOOL isInitV() const    { return _kind == R_INITV;    }
  // Check if Result kind is int constant
  BOOL isIntConst() const { return _kind == R_INTCONST; }

  // Check if flag addrof is set
  BOOL IsAddrOf() const   { return _flag & RF_ADDROF;   }
  // Check if flag array is set
  BOOL IsArray() const    { return _flag & RF_ARRAY;    }
  // Check if flag arrow is set
  BOOL IsArrow() const    { return _flag & RF_ARROW;    }
  // Check if flag const_sym is set
  BOOL IsConstSym() const { return _flag & RF_CONST_SYM;}
  // Check if flag deref is set
  BOOL IsDeref() const    { return _flag & RF_DEREF;    }
  // Check if flag dot is set
  BOOL IsDot() const      { return _flag & RF_DOT;      }
  // Check if flag ref is set
  BOOL IsRef() const      { return _flag & RF_REF;      }
  // Check if flag rvalref is set
  BOOL IsLValue() const   { return _flag & RF_LVALUE;   }
  // Check if flag rvalue is set
  BOOL IsRValue() const   { return _flag & RF_RVALUE;   }
  // Check if flag wn_used is set
  BOOL IsWnUsed() const   { return _flag & RF_WN_USED;  }

  // Set flag addrof
  void SetAddrOf()        { _flag |= RF_ADDROF;         }
  // Set flag arrow
  void SetArray()         { _flag |= RF_ARRAY;          }
  // Set flag arrow
  void SetArrow()         { _flag |= RF_ARROW;          }
  // Set flag const_sym
  void SetConstSym()      { _flag |= RF_CONST_SYM;      }
  // Set flag deref
  void SetDeref()         { _flag |= RF_DEREF;          }
  // Set flag dot
  void SetDot()           { _flag |= RF_DOT;            }
  // Set flag ref
  void SetRef()           { _flag |= RF_REF;            }
  // Set flag rvalref
  void SetLValue()        { _flag |= RF_LVALUE;         }
  // Set flag rvalue
  void SetRValue()        { _flag |= RF_RVALUE;         }
  // Set flag wn_used
  void SetWnUsed()        { _flag |= RF_WN_USED;        }

  // Reset flag addrof
  void ResetAddrOf()      { _flag &= ~RF_ADDROF;        }
  // Reset flag array
  void ResetArray()       { _flag &= ~RF_ARRAY;         }
  // Reset flag arrow
  void ResetArrow()       { _flag &= ~RF_ARROW;         }
  // Reset flag const_sym
  void ResetConstSym()    { _flag &= ~RF_CONST_SYM;     }
  // Reset flag deref
  void ResetDeref()       { _flag &= ~RF_DEREF;         }
  // Reset flag dot
  void ResetDot()         { _flag &= ~RF_DOT;           }
  // Reset flag ref
  void ResetRef()         { _flag &= ~RF_REF;           }
  // Reset flag rvalref
  void ResetLValue()      { _flag &= ~RF_LVALUE;        }
  // Reset flag rvalue
  void ResetRValue()      { _flag &= ~RF_RVALUE;        }
  // Reset flag wn_used
  void ResetWnUsed()      { _flag &= ~RF_WN_USED;       }

  // Increase field id
  void AddFieldId(IDTYPE id) { _fld += id;              }
  // Set field id
  void SetFieldId(IDTYPE id) { _fld = id;               }
  // Set type
  void SetType(TY_IDX ty)    { _ty = ty;                }
  // Set wn
  void SetNode(WN *wn)       { _r._wn = wn;             }

  // Get whirl node as LValue for this Result
  WN*  GetLValue();
  // Get whirl node as RValue for this Result
  WN*  GetRValue();
  // Convert this into RValue and return a new Result
  Result ConvertToRValue(TY_IDX rty);

  // New API proposed
  WN  *GetAddress();
  WN  *GetValue();
  WN  *CreateLoad();
  WN  *CreateStore();

  // compare two Results
  BOOL operator == (const Result & rhs) const {
    return _r._val == rhs._r._val &&
           _ty == rhs._ty &&
           _kind == rhs._kind &&
           _flag == rhs._flag &&
           _fld == rhs._fld;
  }

  // compare two Results
  BOOL operator != (const Result &rhs) const {
    return ! (*this == rhs);
  }

  void dump();
};

}

#endif // CLANG2WHIRL_RESULT_H

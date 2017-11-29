/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//====================================================================
//
// Module: wssa_sym.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Inteface for WHIRL SSA symbol table and version table
//
// Exported classes:
//  WSSA::WST_Symbol_Entry
//  WSSA::WST_Version_Entry
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#ifndef wssa_sym_INCLUDED
#define wssa_sym_INCLUDED

#include "wssa_defs.h"
#include "symtab.h"
#include <vector>
#include <ext/hash_map>
using __gnu_cxx::hash_map;
#include "opt_points_to.h"

namespace WSSA {

//===================================================================
//
// Structure of WSSA symtab
//
// Guideline:
//   WHIRL NODE always reference ST
//   SSA NODE always reference WST/VER
// Structure:
//
//   WHIRL node
//    |
//    | Normal ST:
//    +-> ST->wssa_st_idx ----> WST_Symbol_Entry ----> ST
//    |                          ^        ^        +-> PREG_NUM
//    | PREG ST:                 |        |        +-> WST_Field_Info [WST_FIELD_TABLE]
//    +-> WST_PREG_MAP ----------+        |        +-> WST_Vsym_Info  [WST_VSYM_TABLE]
//                                        |
//                              WST_Version_Entry [SYM_VER_TABLE] 
//                                        ^
//                                        | 
//    SSA node (PHI/CHI/MU, res/opnd) ----+
//
//===================================================================

// field info, includes: offset, size, etc
class WST_Field_Info {
private:
  STR_IDX _name_idx;    // variable.field
  INT32 _field_id;
  INT64 _byte_size;
  INT64 _byte_offset;
  UINT8 _bit_size;
  UINT8 _bit_offset;

public:
  WST_Field_Info();
  WST_Field_Info(INT32 st_idx, INT32 field_id, INT64 byte_size, INT64 byte_offset,
                 UINT8 bit_size, UINT8 bit_offset, INT32 seq_idx);

public:
  STR_IDX Name_idx() const             { return _name_idx;      }
  void   Set_name_idx(STR_IDX idx)     { _name_idx = idx;       }
  INT32  Field_id() const              { return _field_id;      }
  void   Set_field_id(INT32 field_id)  { _field_id = field_id;  }
  INT64  Byte_size() const             { return _byte_size;     }
  void   Set_byte_size(INT64 size)     { _byte_size = size;     }
  INT64  Byte_offset() const           { return _byte_offset;   }
  void   Set_byte_offset(INT64 offset) { _byte_offset = offset; }
  UINT8  Bit_size() const              { return _bit_size;      }
  void   Set_bit_size(UINT8 size)      { _bit_size = size;      }
  UINT8  Bit_offset() const            { return _bit_offset;    }
  void   Set_bit_offset(UINT8 offset)  { _bit_offset = offset;  }

  void   Print(FILE* fp = stdout) const;
};

// vsym info, includes: type, base ST, alias class, etc
class WST_Vsym_Info {
private:
  WSSA_VSYM_TYPE  _vsym_type;
  union {
    STR_IDX _name_idx;  // name of the vsym
    // TODO, alias information
  } _vsym;
  POINTS_TO _points_to;

public:
  WST_Vsym_Info();
  WST_Vsym_Info(WSSA_VSYM_TYPE type);
  WST_Vsym_Info(const WST_Vsym_Info& vsym);
  WST_Vsym_Info& operator=(const WST_Vsym_Info& vsym);

public:
  WSSA_VSYM_TYPE Vsym_type() const    { return _vsym_type;     }
  void Set_vsym_type(WSSA_VSYM_TYPE type)
                                      { _vsym_type = type;     }
  INT32 Name_idx() const {
    return _vsym._name_idx;
  }
  void Set_name_idx(STR_IDX idx) {
    _vsym._name_idx = idx;
  }
  const POINTS_TO& Points_to() const {
    return _points_to;
  }
  POINTS_TO& Points_to() {
    return _points_to;
  }
  void Copy_points_to(const POINTS_TO& pt) {
    _points_to.Copy_fully(&pt);
  }
  void Copy_points_to(const POINTS_TO* pt) {
    _points_to.Copy_fully(pt);
  }

  void Print(FILE* fp = stdout) const;
};

// WST symbol entry
class WST_Symbol_Entry {
private:
  WSSA_SYM_TYPE _sym_type;
  ST_IDX _st_idx;                // Related ST in WHIRL SYMTAB
  WST_IDX _next_wst;             // Next WST related to the same ST
  union {
    PREG_NUM   _preg_num;        // PREG number
    FIELD_INFO_IDX  _field_idx;  // WHIRL Field
    VSYM_INFO_IDX   _vsym_idx;   // virtual symbol
    INT32 _value;                // value for I/O
  } _sym;
  UINT32 _max_ver;               // max version
  VER_IDX _last_ver;             // last version index of the symbol
  // TODO: FFA, POINTS_TO

public:
  INT32 Get_value() const    { return _sym._value; }
  void  Set_value(INT32 idx) { _sym._value = idx;  }

public:
  /* Only WHIRL_SSA_MANAGER can create the instances */
  WST_Symbol_Entry();
  WST_Symbol_Entry(WSSA_SYM_TYPE type);

public:

  WSSA_SYM_TYPE Sym_type() const        { return _sym_type; }
  void Set_sym_type(WSSA_SYM_TYPE type) { _sym_type = type; }

  ST_IDX St_idx() const {
    return _st_idx;
  }
  void Set_st_idx(ST_IDX idx) {
    _st_idx = idx;
  }
  WST_IDX Next_wst() const {
    return _next_wst;
  }
  void Set_next_wst(WST_IDX idx) {
    _next_wst = idx;
  }
  PREG_NUM Preg_num() const {
    Is_True(Sym_type() == WST_PREG, ("Bad sym type"));
    return _sym._preg_num;
  }
  void Set_preg_num(PREG_NUM num) {
    Is_True(Sym_type() == WST_PREG, ("Bad sym type"));
    _sym._preg_num = num;
  }
  FIELD_INFO_IDX Field_idx() const {
    Is_True(Sym_type() == WST_FIELD, ("Bad sym type"));
    return _sym._field_idx;
  }
  void Set_field_idx(FIELD_INFO_IDX idx) {
    Is_True(Sym_type() == WST_FIELD, ("Bad sym type"));
    _sym._field_idx = idx;
  }
  VSYM_INFO_IDX Vsym_idx() const {
    Is_True(Sym_type() == WST_VSYM, ("Bad sym type"));
    return _sym._vsym_idx;
  }
  void Set_vsym_idx(VSYM_INFO_IDX idx) {
    Is_True(Sym_type() == WST_VSYM, ("Bad sym type"));
    _sym._vsym_idx = idx;
  }

  UINT32 Max_ver() const       { return _max_ver; }
  void Set_max_ver(UINT32 ver) { _max_ver = ver;  }
  UINT32 Next_ver() {
    UINT32 ret = _max_ver++; 
    return ret; 
  }

  VER_IDX Last_ver() const       { return _last_ver; }
  void Set_last_ver(VER_IDX ver) { _last_ver = ver; }

public:
  void Print(FILE* fp = stdout) const;
};

// Version flag
enum VER_FLAG {
  VER_IS_ZERO = 0x1,     // zero version
  VER_IS_VOLATILE = 0x2, // def or use is volatile
  VER_BY_OPT_CHI = 0x4,  // def by OPT_CHI
};

// WSSA version entry
class WST_Version_Entry {
private:
  WST_IDX _wst_idx;     // index to wssa symbol table
  UINT32 _version;      // version, can be zero
  VER_IDX _prev_ver;    // previous version index of the same symbol
  const WN* _def_wn;    // WN to define this version
  union {
    struct {
      INT8 _def_type;   // WSSA_NODE_KIND
      INT8 _ver_flags;
    };
    INT32 _flags;
  };
  // TODO: FSA, POINTS_TO

private:
  void Set_flag(VER_FLAG flag)    { _ver_flags |= flag; }
  void Reset_flag(VER_FLAG flag)  { _ver_flags &= !flag; }
  BOOL Is_flag_set(VER_FLAG flag) const { return (_ver_flags & flag) == flag; }
  void Set_wn_flag();

public:
  // these routines are for IO only
  void Set_flags(INT32 flags)     { _flags = flags; }
  INT32 Get_flags() const         { return _flags;  }

public:
  WST_Version_Entry();
  WST_Version_Entry(WST_IDX wst_idx, UINT32 ver,
                    const WN* def_wn, WSSA_NODE_KIND def_type);

public:
  WST_IDX Get_wst() const   { return _wst_idx; }
  void Set_wst(WST_IDX wst) { _wst_idx = wst;  }

  UINT32 Get_ver() const   { return _version; }
  void Set_ver(UINT32 ver) { _version = ver;  }

  const WN* Get_def_wn() const  { return _def_wn; }
  void Set_def_wn(const WN* wn) { 
    _def_wn = wn;   
    Set_wn_flag();
  }

  VER_IDX Prev_ver() const       { return _prev_ver; }
  void Set_prev_ver(VER_IDX ver) { _prev_ver = ver;  }

  WSSA_NODE_KIND Get_def_type() const    { return (WSSA_NODE_KIND)_def_type; }
  void Set_def_type(WSSA_NODE_KIND type) { _def_type = type; }

  void Set_zero()      { Set_flag(VER_IS_ZERO);   }
  void Reset_zero()    { Reset_flag(VER_IS_ZERO); }
  BOOL Is_zero() const { return Is_flag_set(VER_IS_ZERO); }

  void Set_volatile()      { Set_flag(VER_IS_VOLATILE);   }
  void Reset_volatile()    { Reset_flag(VER_IS_VOLATILE); }
  BOOL Is_volatile() const { return Is_flag_set(VER_IS_VOLATILE); }

  void Set_opt_chi()       { Set_flag(VER_BY_OPT_CHI);   }
  void Reset_opt_chi()    { Reset_flag(VER_BY_OPT_CHI); }
  BOOL Is_opt_chi() const { return Is_flag_set(VER_BY_OPT_CHI); }

public:
  void Print(FILE* fp = stdout) const;
};

typedef std::vector<WST_Field_Info>     WST_FIELD_TABLE;
typedef std::vector<WST_Vsym_Info>      WST_VSYM_TABLE;
typedef std::vector<WST_Symbol_Entry>   WST_SYM_TABLE;
typedef std::vector<WST_Version_Entry>  WST_VER_TABLE;

} /* namespace WSSA */

#endif /* wssa_sym_INCLUDED */


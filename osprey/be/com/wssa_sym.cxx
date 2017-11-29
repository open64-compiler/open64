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
// Module: wssa_sym.cxx
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation for WHIRL SSA symbol table and version table
//
// Exported classes:
//  WSSA::WST_Symbol_Entry
//  WSSA::WST_Version_Entry
//
// SEE ALSO:
//  be/com/wssa_sym.h
//
//====================================================================

#include "wssa_sym.h"
#include "wssa_wn.h"
#include "wn.h"

namespace WSSA {

//====================================================================
// WST_Field_Info, attributes for fields in struct
//====================================================================
WST_Field_Info::WST_Field_Info() 
  : _name_idx(0), _field_id(0), 
    _byte_size(0), _byte_offset(0),
    _bit_size(0), _bit_offset(0) {
}

WST_Field_Info::WST_Field_Info(INT32 st_idx, INT32 field_id, INT64 byte_size, INT64 byte_offset,
                               UINT8 bit_size, UINT8 bit_offset, INT32 seq_idx)
  : _field_id(field_id), 
    _byte_size(byte_size), _byte_offset(byte_offset),
    _bit_size(bit_size), _bit_offset(bit_offset) {
  _name_idx = Save_Str2i(ST_name(st_idx), "@", seq_idx);
}

void
WST_Field_Info::Print(FILE* fp) const {
  fprintf(fp, "%s, byte size %lld, byte offset %lld, bit size %d, bit offset %d, field id %d\n",
    (const char*)(&(Str_Table[_name_idx])), _byte_size, _byte_offset, 
    _bit_size, _bit_offset, _field_id);
}

//====================================================================
// WST_Vsym_Info, attributes for virtual symbols
//====================================================================
WST_Vsym_Info::WST_Vsym_Info()
  : _vsym_type(WVT_UNKNOWN) {
  _points_to.Init();
}

WST_Vsym_Info::WST_Vsym_Info(WSSA_VSYM_TYPE type)
  : _vsym_type(type) {
  _points_to.Init();
}

WST_Vsym_Info::WST_Vsym_Info(const WST_Vsym_Info& vsym) {
  Set_vsym_type(vsym.Vsym_type());
  Set_name_idx(vsym.Name_idx());
  Copy_points_to(vsym.Points_to());
}

WST_Vsym_Info&
WST_Vsym_Info::operator=(const WST_Vsym_Info& vsym) {
  Set_vsym_type(vsym.Vsym_type());
  Set_name_idx(vsym.Name_idx());
  Copy_points_to(vsym.Points_to());
  return *this;
}

void
WST_Vsym_Info::Print(FILE* fp) const{
  fprintf(fp, "%s, vsym type is %d\n",
    (const char*)(&(Str_Table[_vsym._name_idx])), _vsym_type);
}

//====================================================================
// WST_Symbol_Entry, attributes fro WSSA symbols
//====================================================================
WST_Symbol_Entry::WST_Symbol_Entry()
  : _sym_type(WST_UNKNOWN), _next_wst(WST_INVALID), 
    _max_ver(WSSA_ZERO_VER), _last_ver(VER_INVALID) { 
}

WST_Symbol_Entry::WST_Symbol_Entry(WSSA_SYM_TYPE type)
  : _sym_type(type), _next_wst(WST_INVALID),
    _max_ver(WSSA_ZERO_VER), _last_ver(VER_INVALID) { 
}

void
WST_Symbol_Entry::Print(FILE* fp) const {
  fprintf(fp, "Type: %s, st_idx: %d, index: %d, max_ver: %d, last_def: %d",
              WSSA_sym_type_name(_sym_type), _st_idx, _sym._value, _max_ver, _last_ver);
}

//====================================================================
// WST_Version_Entry, version information for WSSA symbol
//====================================================================
WST_Version_Entry::WST_Version_Entry()
  : _wst_idx((WST_IDX)INVALID_IDX), _version(INVALID_VER), _prev_ver(VER_INVALID),
    _def_wn(NULL), _def_type(WSSA_UNKNOWN), _ver_flags(0) {
}

WST_Version_Entry::WST_Version_Entry(WST_IDX wst_idx, UINT32 ver, const WN* wn, WSSA_NODE_KIND type)
  : _wst_idx(wst_idx), _version(ver), _prev_ver(VER_INVALID),
    _def_wn(wn), _def_type(type), _ver_flags(0) {
  Set_wn_flag();
}

void
WST_Version_Entry::Set_wn_flag() {
  if (_def_wn != NULL && WSSA::WN_is_volatile(_def_wn))
    Set_volatile();
  else
    Reset_volatile();
  if (_def_wn != NULL && WN_operator(_def_wn) == OPR_OPT_CHI)
    Set_opt_chi();
  else
    Reset_opt_chi();
}

void 
WST_Version_Entry::Print(FILE* fp) const {
  fprintf(fp, "WST_IDX: %d, Ver: %d, Prev Ver: %d, Def: %s:%p, Flag: %x\n",
    _wst_idx, _version, _prev_ver, WSSA_node_name(_def_type), _def_wn, _ver_flags);
}

} /* namespace WSSA */

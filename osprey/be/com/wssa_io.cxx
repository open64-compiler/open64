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
// Module: wssa_io.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Implementation of I/O for WHIRL SSA
//
// Exported classes:
//  WSSA::WHIRL_SSA_IO
//
// SEE ALSO:
//  be/com/wssa_io.h
//
//====================================================================

#include "wssa_io.h"

#ifndef __GNUC__
#define __ALIGNOF(x) __builtin_alignof(x)
#else
#define __ALIGNOF(x) __alignof__(x)
#endif

namespace WSSA {

//====================================================================
// wrappers for input buffer and raw read/write routines
//====================================================================

// wrap the input buffer
struct Input_Buffer {
  WHIRL_SSA_MANAGER* _mgr;
  char* _base;
  char* _current;
  Input_Buffer(WHIRL_SSA_MANAGER* mgr, char* ptr) 
    : _mgr(mgr), _base(ptr), _current(ptr) { }
};

// wrap the output file
struct Output_Buffer {
  const WHIRL_SSA_MANAGER* _mgr;
  Output_File *_fl;
  Output_Buffer(const WHIRL_SSA_MANAGER* mgr, Output_File* fl)
    : _mgr(mgr), _fl(fl) { }
};

// ir_b_read_buf for any type of data using INT32 alignment
template<typename _Tp>
void ir_b_read_buf(Input_Buffer *ib, _Tp& data) {
  data = * (_Tp*) ib->_current;
  if (sizeof(_Tp) % __ALIGNOF(INT32) == 0)
    ib->_current += sizeof(_Tp);
  else
    ib->_current += (sizeof(_Tp)/__ALIGNOF(INT32) + 1) * __ALIGNOF(INT32);
}


template<typename _Tp>
void write_to_file(Output_Buffer *ob, const _Tp& data);

template<typename _Tp>
void read_from_buffer(Input_Buffer *ib, _Tp& data);

//====================================================================
// write/read builtin data types 
//====================================================================

// specialized write_to_file for writing INT32
template<>
void write_to_file(Output_Buffer *ob, const INT32& data) {
  ir_b_save_buf(&data, sizeof(INT32), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading INT32
template<>
void read_from_buffer(Input_Buffer *ib, INT32& data) {
  ir_b_read_buf(ib, data);
}

// specialized write_to_file for writing UINT32
template<>
void write_to_file(Output_Buffer *ob, const UINT32& data) {
  ir_b_save_buf(&data, sizeof(UINT32), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading UINT32
template<>
void read_from_buffer(Input_Buffer *ib, UINT32& data) {
  ir_b_read_buf(ib, data);
}

// specialized write_to_file for writing VER_IDX
template<>
void write_to_file(Output_Buffer *ob, const VER_IDX& data) {
  ir_b_save_buf(&data, sizeof(UINT32), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading VER_IDX
template<>
void read_from_buffer(Input_Buffer *ib, VER_IDX& data) {
  INT32 ival;
  ir_b_read_buf(ib, ival);
  data = (VER_IDX)ival;
}

//====================================================================
// I/O routines for SSA_NODE_BASE
//====================================================================

template<typename _Tnode>
void write_to_file(Output_Buffer *ob, _Tnode* const & node) {
  if (_Tnode::NODE_KIND == WSSA_PHI) {
    write_to_file(ob, node->Opnd_count());
  }
  for (int res = 0; res < node->Res_count(); ++res) {
    write_to_file(ob, node->Get_res(res));
  }
  for (int opnd = 0; opnd < node->Opnd_count(); ++opnd) {
    write_to_file(ob, node->Get_opnd(opnd));
  }
}

template<typename _Tnode>
void read_from_buffer(Input_Buffer *ib, _Tnode*& node) {
  INT32 opnd_count = 0;
  if (_Tnode::NODE_KIND == WSSA_PHI) {
    read_from_buffer(ib, opnd_count);
  }
  node = ib->_mgr->template Create_node<_Tnode::NODE_KIND>(opnd_count);

  for (int res = 0; res < node->Res_count(); ++res) {
    VER_IDX data;
    read_from_buffer(ib, data);
    node->Set_res(res, data);
  }
  for (int opnd = 0; opnd < node->Opnd_count(); ++opnd) {
    VER_IDX data;
    read_from_buffer(ib, data);
    node->Set_opnd(opnd, data);
  }
}

//===================================================================
// write/read generic stl lists/maps
//===================================================================

// specialized write_to_file for writing std::vector
template<typename _Tp, typename _Alloc>
void write_to_file(Output_Buffer *ob, const std::vector<_Tp, _Alloc>& data) {
  INT32 vec_size = data.size();
  write_to_file(ob, vec_size);
  typename std::vector<_Tp, _Alloc>::const_iterator it;
  for (it = data.begin(); it != data.end(); ++it) {
    write_to_file(ob, *it);
  }
}

// specialized read_from_buffer for reading std::vector
template<typename _Tp, typename _Alloc>
void read_from_buffer(Input_Buffer *ib, std::vector<_Tp, _Alloc>& data) {
  INT32 vec_size;
  read_from_buffer(ib, vec_size);
  data.reserve(vec_size);
  for (int i=0; i<vec_size; ++i) {
    _Tp element;
    read_from_buffer(ib, element);
    data.push_back(element);
  }
  Is_True(data.size() == vec_size, ("size mismatch"));
}

// specialized write_to_file for writing __gnu_cxx::hash_map
template<typename _Val, typename _Tp, 
         typename _HashFcn, typename _HashEq, typename _Alloc>
void write_to_file(Output_Buffer *ob, const hash_map<_Val, _Tp, _HashFcn, _HashEq, _Alloc>& data) {
  INT32 map_size = data.size();
  write_to_file(ob, map_size);
  typename hash_map<_Val, _Tp, _HashFcn, _HashEq, _Alloc>::const_iterator it;
  for (it = data.begin(); it != data.end(); ++it) {
    write_to_file(ob, it->first);
    write_to_file(ob, it->second);
  }
}

// specialized read_from_buffer for reading __gnu_cxx::hash_map
template<typename _Val, typename _Tp, 
         typename _HashFcn, typename _HashEq, typename _Alloc>
void read_from_buffer(Input_Buffer *ib, hash_map<_Val, _Tp, _HashFcn, _HashEq, _Alloc>& data) {
  INT32 map_size;
  read_from_buffer(ib, map_size);
  for (int i=0; i < map_size; ++i) {
    _Val key;
    _Tp val;
    read_from_buffer(ib, key);
    read_from_buffer(ib, val);
    data[key] = val;
  }
  Is_True(data.size() == map_size, ("size mismatch"));
}

//===================================================================
// write/read WSSA data types
//===================================================================

// specialized write_to_file for writing WSSA::WST_Field_Info
template<>
void write_to_file(Output_Buffer *ob, const WST_Field_Info& field) {
  ir_b_save_buf(&field, sizeof(WST_Field_Info), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading WSSA::WST_Field_Info
template<>
void read_from_buffer(Input_Buffer *ib, WST_Field_Info& field) {
  ir_b_read_buf(ib, field);
}

// specialized write_to_file for writing WSSA::WST_Vsym_Info
template<>
void write_to_file(Output_Buffer *ob, const WST_Vsym_Info& vsym) {
  ir_b_save_buf(&vsym, sizeof(WST_Vsym_Info), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading WSSA::WST_Vsym_Info
template<>
void read_from_buffer(Input_Buffer *ib, WST_Vsym_Info& vsym) {
  ir_b_read_buf(ib, vsym);
}

// specialized write_to_file for writing WSSA::WST_Symbol_Entry
template<>
void write_to_file(Output_Buffer *ob, const WST_Symbol_Entry& sym) {
  ir_b_save_buf(&sym, sizeof(WST_Symbol_Entry), __ALIGNOF(INT32), 0, ob->_fl);
}

// specialized read_from_buffer for reading WSSA::WST_Symbol_Entry
template<>
void read_from_buffer(Input_Buffer *ib, WST_Symbol_Entry& sym) {
  ir_b_read_buf(ib, sym);
}

// specialized write_to_file for writing WSSA::WST_Version_Entry
template<>
void write_to_file(Output_Buffer *ob, const WST_Version_Entry& ver) {
  write_to_file(ob, (INT32)ver.Get_wst());
  write_to_file(ob, (INT32)ver.Get_ver());
  const WN* def_wn = ver.Get_def_wn();
  write_to_file(ob, (INT32)(def_wn ? ob->_mgr->WN_idx(def_wn) : 0));
  write_to_file(ob, (INT32)ver.Get_flags());
}

// specialized read_from_buffer for reading WSSA::WST_Version_Entry
template<>
void read_from_buffer(Input_Buffer *ib, WST_Version_Entry& ver) {
  INT32 data;
  read_from_buffer(ib, data);
  ver.Set_wst((WST_IDX)data);
  read_from_buffer(ib, data);
  ver.Set_ver((UINT32)data);
  read_from_buffer(ib, data);
  ver.Set_def_wn(data ? ib->_mgr->Get_wn(data) : NULL);
  read_from_buffer(ib, data);
  ver.Set_flags(data);
}


//====================================================================
// write/read driver
//====================================================================

void
WHIRL_SSA_IO::Write_To_Output_File(Output_File *fl) {
  Output_Buffer ob(_mgr, fl);

  write_to_file(&ob, _mgr->PHI_table());
  write_to_file(&ob, _mgr->CHI_table());
  write_to_file(&ob, _mgr->MU_table());

  write_to_file(&ob, _mgr->WN_phi_map());
  write_to_file(&ob, _mgr->WN_chi_map());
  write_to_file(&ob, _mgr->WN_mu_map());
  write_to_file(&ob, _mgr->WN_ver_map());

  write_to_file(&ob, _mgr->Field_table());
  write_to_file(&ob, _mgr->Vsym_table());
  write_to_file(&ob, _mgr->Sym_table());

  write_to_file(&ob, _mgr->Ver_table());

  write_to_file(&ob, (INT32)_mgr->Default_vsym());
  write_to_file(&ob, (INT32)_mgr->Return_vsym());
}

void 
WHIRL_SSA_IO::Read_SSA_From_File(char *base) {
  Input_Buffer ib(_mgr, base);

  read_from_buffer(&ib, _mgr->PHI_table());
  read_from_buffer(&ib, _mgr->CHI_table());
  read_from_buffer(&ib, _mgr->MU_table());

  read_from_buffer(&ib, _mgr->WN_phi_map());
  read_from_buffer(&ib, _mgr->WN_chi_map());
  read_from_buffer(&ib, _mgr->WN_mu_map());
  read_from_buffer(&ib, _mgr->WN_ver_map());

  read_from_buffer(&ib, _mgr->Field_table());
  read_from_buffer(&ib, _mgr->Vsym_table());
  read_from_buffer(&ib, _mgr->Sym_table());

  read_from_buffer(&ib, _mgr->Ver_table());

  INT32 data;
  read_from_buffer(&ib, data);
  _mgr->Set_default_vsym((WST_IDX)data);
  read_from_buffer(&ib, data);
  _mgr->Set_return_vsym((WST_IDX)data);

  // clean the WN_MAP since it's no longer used
  _mgr->Clear_wn_map();

  // Set the SSA manager state
  _mgr->Set_stat(STAT_OK);
}

} //end of namespace


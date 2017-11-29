/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

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
#ifndef cse_table_INCLUDED
#define cse_table_INCLUDED

#include "defs.h"
#include "wn.h"
#include "intrn_info.h"
#include <ext/hash_map>
using __gnu_cxx::hash_map;
using __gnu_cxx::hash;


enum CSE_PAR_ATTR
  {
    CPA_one_level_read    = 0x00000001,
    CPA_two_level_read    = 0x00000002,
    CPA_multi_level_read  = 0x00000004,
    CPA_one_level_write   = 0x00000008,
    CPA_two_level_write   = 0x00000010,
    CPA_multi_level_write = 0x00000020,
    CPA_exposed_to_globals= 0x00000040,
    CPA_exposed_to_return = 0x00000080,
    // For heap allocator that returns heap
    // address via reference formal:
    CPA_is_pointer_to_heap_addr_loc = 0x00000100,
    CPA_is_format_string  =  0x00000200, /* for printf_like funcs */
    CPA_is_not_escaping   =  0x00000400, /* points-to, escape analysis treats */
                                         /* parm as not escaping              */

    // Default conservative value
    CPA_default_attr  =  
    ((CPA_one_level_read     | 
      CPA_two_level_read     |
      CPA_multi_level_read   | 
      CPA_one_level_write    | 
      CPA_two_level_write    |
      CPA_multi_level_write  | 
      CPA_exposed_to_globals |
      CPA_exposed_to_return)      &
     ~CPA_is_pointer_to_heap_addr_loc &
     ~CPA_is_format_string &
     ~CPA_is_not_escaping),

    CPA_no_ptr_deref_and_expose  = 0,

    CPA_end
  };

enum CSE_FUNC_ATTR
  {
    CFAT_argument_indirectly_read            = 0x00000001,
    CFAT_argument_indirectly_write           = 0x00000002,
    CFAT_globals_read                        = 0x00000004,
    CFAT_globals_write                       = 0x00000008,
    CFAT_libc_globals_read                   = 0x00000010,
    CFAT_libc_globals_write                  = 0x00000020,
    CFAT_libcpp_globals_read                 = 0x00000040,
    CFAT_libcpp_globals_write                = 0x00000080,
    CFAT_statics_read                        = 0x00000100,
    CFAT_statics_write                       = 0x00000200,
    CFAT_exposed_memory_read                 = 0x00000400,
    CFAT_exposed_memory_write                = 0x00000800,
    CFAT_hidden_data_read                    = 0x00001000,
    CFAT_hidden_data_write                   = 0x00002000,
    CFAT_allocates_heap_memory               = 0x00004000,
    // allocated memory is passed back via return ptr. 
    CFAT_returns_heap_memory                 = 0x00008000,
    CFAT_deallocates_heap_memory             = 0x00010000,
    CFAT_exposes_argument_address_to_return  = 0x00020000,
    CFAT_exposes_argument_address_to_globals = 0x00040000,
    CFAT_argument_one_level_deref            = 0x00080000,
    CFAT_is_marked_libcall                   = 0x00100000,
    CFAT_callee_kind_mask                    = 0x00e00000,
    // The following attributes are mutually exclusive, 
    // or inclusive  so use only 3 bits
    CFAT_is_libc_entry                       = 0x00200000,
    CFAT_is_libcpp_entry                     = 0x00400000,
    CFAT_is_dflt_ctor                        = 0x00600000,
    CFAT_is_dflt_cpy_ctor                    = 0x00800000,
    CFAT_is_lib_f90                          = 0x00a00000,
    CFAT_is_asm_intrin                       = 0x00c00000,
    // more attributes here:
    CFAT_has_format_string                   = 0x01000000,
    CFAT_is_printf_like                      = 0x02000000,
    // return parameter is exposed
    CFAT_returns_exposed_memory              = 0x10000000,
    CFAT_returns_non_pointer                 = 0x20000000,
    CFAT_return_is_not_escaping              = 0x40000000,

    // some composite defaults:
    CFAT_libc_default_attr =  (
                               CFAT_libc_globals_read  |
                               CFAT_libc_globals_write |
                               CFAT_hidden_data_read   |
                               CFAT_hidden_data_write  |
                               CFAT_is_libc_entry ),
    CFAT_libcpp_default_attr =  (
                                 CFAT_libcpp_globals_read  |
                                 CFAT_libcpp_globals_write |
                                 CFAT_libc_globals_read    |
                                 CFAT_libc_globals_write   |
                                 CFAT_hidden_data_read     |
                                 CFAT_hidden_data_write    |
                                 CFAT_is_libcpp_entry ),
    CFAT_libf90_default_attr =  (
                                 CFAT_hidden_data_read   |
                                 CFAT_hidden_data_write  |
                                 CFAT_is_lib_f90 ),

    // Conservative default
    CFAT_reads_all_attr  = (
                            CFAT_argument_indirectly_read |
                            CFAT_globals_read             |
                            CFAT_libc_globals_read        | 
                            CFAT_libcpp_globals_read      | 
                            CFAT_statics_read             | 
                            CFAT_exposed_memory_read      | 
                            CFAT_hidden_data_read),
    CFAT_writes_all_attr  = (
                             CFAT_argument_indirectly_write |
                             CFAT_globals_write             |
                             CFAT_libc_globals_write        | 
                             CFAT_libcpp_globals_write      | 
                             CFAT_statics_write             | 
                             CFAT_exposed_memory_write      | 
                             CFAT_hidden_data_write),
    CFAT_touches_all_attr = (
                             CFAT_reads_all_attr  |
                             CFAT_writes_all_attr |
                             CFAT_exposes_argument_address_to_return |
                             CFAT_exposes_argument_address_to_globals |
                             CFAT_returns_exposed_memory),
        
    CFAT_default_attr = (
                         CFAT_touches_all_attr & 
                         ~CFAT_allocates_heap_memory & 
                         ~CFAT_returns_heap_memory &
                         ~CFAT_argument_one_level_deref & 
                         ~CFAT_is_marked_libcall &
                         ~CFAT_has_format_string & 
                         ~CFAT_deallocates_heap_memory &
                         ~CFAT_returns_non_pointer &
                         ~CFAT_return_is_not_escaping),

    // 
    // Pure call return value depends only on input argument -- it does not
    // depend on any global states (hidden included). Note that global read-
    // only data are not considered global data nor hidden data.
    // 
    CFAT_pure_call_attr_mask = CFAT_touches_all_attr,
    // 
    // Safe calls can touch global states, but only perform reading -- they do
    // not define any global states -- safe calls are allowed to be CSEed.
    // 
    CFAT_safe_call_attr_mask = (
                                CFAT_writes_all_attr |
                                CFAT_exposes_argument_address_to_return |
                                CFAT_exposes_argument_address_to_globals | 
                                CFAT_allocates_heap_memory |
                                CFAT_deallocates_heap_memory),
        
    CFAT_end
  };

inline bool cfa_read_globals(UINT32 s)  { return (s & CFAT_globals_read);  }
inline bool cfa_write_globals(UINT32 s) { return (s & CFAT_globals_write); }
inline bool cfa_write_statics(UINT32 s) { return (s & CFAT_statics_write); }
inline bool cfa_read_statics(UINT32 s)  { return (s & CFAT_statics_read);  }
inline bool cfa_read_hidden_data(UINT32 s)  { return (s & CFAT_hidden_data_read);  }
inline bool cfa_write_hidden_data(UINT32 s) { return (s & CFAT_hidden_data_write); }
inline bool cfa_is_heap_allocating(UINT32 s){ return (s & CFAT_allocates_heap_memory); }
inline bool cfa_returns_heap_memory(UINT32 s){ return (s & CFAT_returns_heap_memory);  }
inline bool cfa_is_heap_deallocating(UINT32 s) { return (s & CFAT_deallocates_heap_memory);}
inline bool cfa_read_libc_globals(UINT32 s) 
{ 
  return cfa_read_globals(s) || (s & CFAT_libc_globals_read); 
}
inline bool cfa_write_libc_globals(UINT32 s) 
{ 
  return cfa_write_globals(s) || (s & CFAT_libc_globals_write); 
}
inline bool cfa_read_libcpp_globals(UINT32 s) 
{ 
  return cfa_read_globals(s) || (s & CFAT_libcpp_globals_read); 
}
inline bool cfa_write_libcpp_globals(UINT32 s) 
{ 
  return cfa_write_globals(s) || (s & CFAT_libcpp_globals_write); 
}
inline bool cfa_read_exposed_memory(UINT32 s)  { return (s & CFAT_exposed_memory_read);}
inline bool cfa_write_exposed_memory(UINT32 s) { return (s & CFAT_exposed_memory_write);}
inline bool cfa_read_arg_indirectly(UINT32 s)  { return (s & CFAT_argument_indirectly_read);}
inline bool cfa_write_arg_indirectly(UINT32 s) { return (s & CFAT_argument_indirectly_write);}
inline bool cfa_expose_arg_address_to_return (UINT32 s) { return s & CFAT_exposes_argument_address_to_return; }
inline bool cfa_expose_arg_address_to_globals (UINT32 s) { return s & CFAT_exposes_argument_address_to_globals; }
inline bool cfa_is_marked_libcall(UINT32 s) { return (s & CFAT_is_marked_libcall); }
inline bool cfa_is_printf_like(UINT32 s)    { return (s & CFAT_is_printf_like);    }
inline bool cfa_has_format_string(UINT32 s) { return (s & CFAT_has_format_string); }
inline bool cfa_is_stdlibc_entry(UINT32 s) { return (s & CFAT_callee_kind_mask) == CFAT_is_libc_entry;}
inline bool cfa_is_libcpp_entry(UINT32 s) { return (s & CFAT_callee_kind_mask) == CFAT_is_libcpp_entry;}
inline bool cfa_is_f90_entry(UINT32 s) { return (s & CFAT_callee_kind_mask) == CFAT_is_lib_f90;}
inline bool cfa_returns_exposed_memory(UINT32 s){ return (s & CFAT_returns_exposed_memory);  }
inline bool cfa_returns_non_pointer(UINT32 s) { return (s & CFAT_returns_non_pointer); }
inline bool cfa_return_is_not_escaping(UINT32 s) { return (s & CFAT_return_is_not_escaping); }

// pure func
inline bool cfa_is_pure(UINT32 s) {
  return (s & CFAT_pure_call_attr_mask) == 0;
}

inline void cfa_set_is_pure(UINT32& s) {
  (s &= (~CFAT_pure_call_attr_mask));
}

// A safe function is candidate for CSE. A pure func is 
// also safe, but not vice-versa.
inline bool cfa_is_safe(UINT32 s) {
  return (s & CFAT_safe_call_attr_mask) == 0;
}

extern bool
doesFormatStringContainPercN(WN* call_node, UINT32 format_arg_pos);

const UINT32 C_max_cse_arg_num = 6; // TODO
struct CallSideEffectInfoBase
{
  const char*  FuncName;
  UINT32       SideEffects;
  UINT8        NumOfKnownPars;
  UINT32       ParAttrs[C_max_cse_arg_num];
};

class CallSideEffectInfo : protected CallSideEffectInfoBase
{
public:
  static CallSideEffectInfo GetDefaultCallSideEffectInfo(const WN* wn);
  static CallSideEffectInfo GetCallSideEffectInfo(const WN* call_node,
                                                  bool* from_table = NULL);
  static CallSideEffectInfo GetCallSideEffectInfo(const ST* sym,
                                                  bool* from_table = NULL);
  static CallSideEffectInfo GetCallSideEffectInfo(const INTRINSIC intr_id,
                                                  bool *from_table = NULL);

  bool isPureCall()         const { return cfa_is_pure(SideEffects); }
  bool isSafeCall()         const { return cfa_is_safe(SideEffects); }
  bool readGlobals()        const { return cfa_read_globals(SideEffects);  }
  bool readStatics()        const { return cfa_read_statics(SideEffects);  }
  bool writeGlobals()       const { return cfa_write_globals(SideEffects); }
  bool writeStatics()       const { return cfa_write_statics(SideEffects); }
  bool readHiddenData()     const { return cfa_read_hidden_data(SideEffects);    } 
  bool writeHiddenData()    const { return cfa_write_hidden_data(SideEffects);   } 
  bool isHeapAllocating()   const { return cfa_is_heap_allocating(SideEffects);  } 
  bool isHeapDeallocating() const { return cfa_is_heap_deallocating(SideEffects);}
  bool returnsHeapMemory()  const { return cfa_returns_heap_memory(SideEffects); }
  bool isStdLibcEntry()     const { return cfa_is_stdlibc_entry(SideEffects);    }
  bool isLibcppEntry()      const { return cfa_is_libcpp_entry(SideEffects);    }
  bool isF90Entry()         const { return cfa_is_f90_entry(SideEffects);    }
  bool readLibcGlobals()    const { return cfa_read_libc_globals(SideEffects); }
  bool writeLibcGlobals()   const { return cfa_write_libc_globals(SideEffects); }
  bool readExposedMemory()  const { return cfa_read_exposed_memory(SideEffects); }
  bool writeExposedMemory() const { return cfa_write_exposed_memory(SideEffects); }
  bool readArgIndirectly()  const { return cfa_read_arg_indirectly(SideEffects); }
  bool writeArgIndirectly() const { return cfa_write_arg_indirectly(SideEffects); }
  bool isPrintfLike()       const { return cfa_is_printf_like(SideEffects); }
  bool exposeArgAddressToReturn() const { return cfa_expose_arg_address_to_return(SideEffects);}
  bool exposeArgAddressToGlobals() const { return cfa_expose_arg_address_to_globals(SideEffects);}
  bool returnsExposedMemory() const { return cfa_returns_exposed_memory(SideEffects);}
  bool returnsNonPointer() const { return cfa_returns_non_pointer(SideEffects); }
  bool returnIsNotEscaping() const { return cfa_return_is_not_escaping(SideEffects); }

  UINT32 GetArgumentAttr(UINT32 arg_pos, WN* call_node = NULL, 
                         bool ignore_format_string = false) const;
  void Print(WN*) const;

  friend class CallSideEffectInfoTable;
    
private:
  CallSideEffectInfo() { FuncName = 0; SideEffects = CFAT_default_attr; NumOfKnownPars = 0;}
  CallSideEffectInfo(CallSideEffectInfoBase&);
  CallSideEffectInfo(const WN* call_node);
  UINT32 GetArgumentAttr_() const;
  static CallSideEffectInfo GetCallSideEffectInfo_(const char* func_name, 
                                                   const WN* call_node,
                                                   bool* from_table);

  bool isDefault_() 
  { return 
      ((SideEffects &~CFAT_callee_kind_mask) == CFAT_default_attr) && NumOfKnownPars == 0 ;}
  void SetIsMarkedLibcall_() { SideEffects |= CFAT_is_marked_libcall; }
  bool isMarkedLibcall_() { return (SideEffects& CFAT_is_marked_libcall);}
  void SetNotExposingAddress_() 
  {
    SideEffects &= (~(CFAT_exposes_argument_address_to_return |
                      CFAT_exposes_argument_address_to_globals));
  }
  void SetIsPure_()
  {
    cfa_set_is_pure(SideEffects);
  }
  void SetNotReadStatics_()  { SideEffects &= (~CFAT_statics_read); }
  void SetNotWriteStatics_() { SideEffects &= (~CFAT_statics_write);}
};

class CallSideEffectInfoTable
{
public:
  CallSideEffectInfoTable();

  void Add(const char* func, CallSideEffectInfo* info)
  {
    SideEffectTable_[func] = info;
  }

  CallSideEffectInfo* Find(const char* func)
  {
    SideEffectTableType_::iterator itr = SideEffectTable_.find(func);
    if (itr == SideEffectTable_.end()) return 0;
    return (CallSideEffectInfo*) itr->second;
  }

private:
  

  struct CIPN 
  {
    UINT32 CipNum;
    const char* NormName;
  };
    
  const char* GetCIPNormName_(UINT32 cip_num, const char* orig_nm);

  class StringEqual_
  {
  public:
    bool operator() (const char* id1, const char* id2) const
    { return !strcmp(id1, id2); }
  };

  typedef hash_map<const char*, 
                   CallSideEffectInfo*, 
                   hash<const char*>, StringEqual_ > SideEffectTableType_;
  // Standard library call side-effect table -- implied by their semantics:
  SideEffectTableType_ SideEffectTable_;
};

#endif

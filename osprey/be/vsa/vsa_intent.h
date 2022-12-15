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
// Module: opt_vsa.h
// $Revision: 1.1.1.1 $
// $Date: 2018/8/14 19:00:00 $
// $Author: $
// $Source: vsa_intent.h,v $
//
// ====================================================================
//

#ifndef vsa_intent_INCLUDED
#define vsa_intent_INCLUDED "vsa_intent.h"

enum LANG_HANDLE  { PROC = 1, PTR_VAR = 2, VAL_VAR = 4 };

#define CAT_NONE        0x00000000
#define ALLOC           0x00000001
#define DEALLOC         0x00000002
#define BLOCK           0x00000004    // MMAP
#define HEAP            0x00000008
#define STK             0x00000010
#define IPURE           0x00000020
#define NOT_USED        0x00000040
#define NORETURN        0x00000080
#define VARNAME         0x00000100
#define PROCNAME        0x00000200
#define IO_RD           0x00000400
#define IO_WR           0x00000800
#define IO_VOLATILE     0x00001000
#define IO_ADDRMAP      0x00002000
#define IGN             0x00004000
#define PART_IGN        0x00008000
#define RET_VITAL       0x00010000
#define SYS_RSRC        0x00020000
#define ALLOCINIT       0x00040000
#define TAINTED         0x00080000
#define PARMRET         0x00100000
#define PARMREG         0x00200000
#define PARMVAR         0x00400000  // parm, LDA
#define PARMVSYM        0x00800000  // parm, LDID pointer
#define REGEX_IGN       0x01000000  // regex


#define IGNORE_CAT_MASK (IGN|PART_IGN|REGEX_IGN)
#define IO_CAT_MASK     0x00003C00
#define PROC_CAT_MASK   0x000003E0
#define MEM_CAT_MASK    0x0000001F
#define RETCHK_CAT_MASK 0x00030000
#define TAINT_PARM_MASK (PARMRET|PARMREG|PARMVAR|PARMVSYM)

#define MEM_CAT_SHIFT      0
#define PROC_CAT_SHIFT     5
#define IO_CAT_SHIFT      10
#define IGNORE_CAT_SHIFT  14
#define RETCHK_CAT_SHIFT  16


typedef enum major_type {
  MEM_ACC      = 0x00000001,
  IO_ACC       = 0x00000002,
  ARRAY_ASSIGN = 0x00000004,
  PROC_EFF     = 0x00000008,
  PARA_EFF     = 0x00000010,
  IGNORE_RES   = 0x00000020,
  CHK_RET      = 0x00000040,
} MAJOR_TYPE;

typedef enum taint_model {
  TM_ARG_RET  = 0,
  TM_ARG_REG  = 1,
  TM_ARG_VAR  = 2,
  TM_ARG_VSYM = 4,
  TM_NONE     = 0xffff,
} TAINTMODEL;

typedef enum alias_intent {
  NO_ALIAS     = 0x0000,  // no alias
  MOD_PARM_0   = 0x0001,  // modify first prameter
  MOD_PARM_1   = 0x0002,  // modify second parameter
  MOD_PARM_2   = 0x0004,  // modify third parameter
  MOD_PARM_3   = 0x0008,  // modify forth parameter
  MOD_PARM_4   = 0x0010,  // modify fifth parameter
  MOD_VARARG   = 0x0020,  // modify vararg parameter
  MOD_PARM     = 0x003f,  // modify parameter
  REF_PARM_0   = 0x0040,  // reference first prameter
  REF_PARM_1   = 0x0080,  // reference second parameter
  REF_PARM_2   = 0x0100,  // reference third parameter
  REF_PARM_3   = 0x0200,  // reference forth parameter
  REF_PARM_4   = 0x0400,  // reference fifth parameter
  REF_VARARG   = 0x0800,  // reference vararg parameter
  REF_PARM     = 0x0fc0,  // reference parameter
  ALIAS_PARM   = 0x0fff,  // alias with parameter
  ALIAS_VSYM   = 0x1000,  // alias with default/return vsym
  ALIAS_WOPT   = 0x2000,  // using wopt alias setting
} ALIAS_INTENT;

inline
ALIAS_INTENT Get_parm_alias_intent(ALIAS_INTENT intent, INT32 idx)
{
  ALIAS_INTENT vararg_intent = (ALIAS_INTENT) (MOD_VARARG | REF_VARARG);
  switch (idx) {
    case 0:
      return (ALIAS_INTENT) (intent & (MOD_PARM_0 | REF_PARM_0 | vararg_intent));
    case 1:
      return (ALIAS_INTENT) (intent & (MOD_PARM_1 | REF_PARM_1 | vararg_intent));
    case 2:
      return (ALIAS_INTENT) (intent & (MOD_PARM_2 | REF_PARM_2 | vararg_intent));
    case 3:
      return (ALIAS_INTENT) (intent & (MOD_PARM_3 | REF_PARM_3 | vararg_intent));
    case 4:
      return (ALIAS_INTENT) (intent & (MOD_PARM_4 | REF_PARM_4 | vararg_intent));
    default:
      return (ALIAS_INTENT) (intent & vararg_intent);
  }
}

struct INTENT_DESC {
  //public:

  MAJOR_TYPE  _mtyp;
  INT         _cat;
  LANG_HANDLE _hdl;
  //  char*       _name;
 
  //  INTENT_DESC(void);
  //  INTENT_DESC(const INTENT_DESC&);
  //  INTENT_DESC& operator = (const INTENT_DESC&);
  //public:
  MAJOR_TYPE  Get_mtyp(void)          const { return _mtyp; }
  LANG_HANDLE Get_hdl(void)           const { return _hdl; }
  //  char*       Get_name(void)          const { return _name; }
  INT32       Get_cat(void)           const { return _cat; }
  INT         Get_IO_cat(void)        const { return _cat & IO_CAT_MASK; }
  INT         Get_MEM_cat(void)       const { return _cat & MEM_CAT_MASK; }
  INT         Get_PROC_cat(void)      const { return _cat & PROC_CAT_MASK; }
  INT         Get_IGNORE_cat(void)    const { return _cat & IGNORE_CAT_MASK; }
  void        Mtyp(MAJOR_TYPE mt)     { _mtyp = mt; }
  void        Hdl(LANG_HANDLE h)      { _hdl = h; }
  // void        Name(char* n)           { _name = n; }
  void        Mem_cat(INT c)          { _cat = (c << MEM_CAT_SHIFT); }
  void        IO_cat(INT c)           { _cat = (c << IO_CAT_SHIFT); }
  void        Proc_cat(INT c)         { _cat = (c << PROC_CAT_SHIFT); }
  void        Ignore_cat(INT c)       { _cat = (c << IGNORE_CAT_SHIFT); }

  //  BOOL Is_func_alloc(INTENT_DESC& it, char* fn); 
  //  BOOL Is_func_dealloc(INTENT_DESC& it, char* fn); 
};

struct ITENT_DESC {
  //public:

  MAJOR_TYPE   _mtyp: 8;
  LANG_HANDLE  _hdl: 8;
  INT          _alias: 16;
  INT          _cat;
  const char  *_name;

  MAJOR_TYPE   Get_mtyp(void)          const { return _mtyp; }
  LANG_HANDLE  Get_hdl(void)           const { return _hdl; }
  ALIAS_INTENT Get_alias(void)         const { return (ALIAS_INTENT)_alias; }
  const char  *Get_name(void)          const { return _name; }
  INT32        Get_cat(void)           const { return _cat; }
  INT          Get_IO_cat(void)        const { return _cat & IO_CAT_MASK; }
  INT          Get_MEM_cat(void)       const { return _cat & MEM_CAT_MASK; }
  INT          Get_PROC_cat(void)      const { return _cat & PROC_CAT_MASK; }
  INT          Get_IGNORE_cat(void)    const { return _cat & IGNORE_CAT_MASK; }
  void         Mtyp(MAJOR_TYPE mt)     { _mtyp = mt; }
  void         Hdl(LANG_HANDLE h)      { _hdl = h; }
  void         Name(char* n)           { _name = n; }
  void         Mem_cat(INT c)          { _cat = (c << MEM_CAT_SHIFT); }
  void         IO_cat(INT c)           { _cat = (c << IO_CAT_SHIFT); }
  void         Proc_cat(INT c)         { _cat = (c << PROC_CAT_SHIFT); }
  void         Ignore_cat(INT c)       { _cat = (c << IGNORE_CAT_SHIFT); }

};

#endif // vsa_intent_INCLUDED

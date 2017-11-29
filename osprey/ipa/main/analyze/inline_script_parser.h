/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.

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

#ifndef ISP_file_INCLUD
#define ISP_file_INCLUD

#include <ipc_option.h>
// commented out as it conflicts with the definition in ipc_option.h
// struct eqstr
// {
//   bool operator()(const char* s1, const char* s2) const
//   {
//    return strcmp(s1, s2) == 0;
//  }
// };

// locally defined macro to control if the verifier is enabled
//#define Enable_ISP_Verify
//#define ISP_DEBUG

// -----------------------------------------------------------------------------------------------
// Definition of the data structure to store the inlining record extracted from the description file
// Basically it's a two level hash-map for fast look-up
// The key of the caller map is composed by caller_def_filename+caller_funcname
// The key of the callee map is composed by callsite_linenum+callee_def_filename+callee_funcname
//
// To Do: to support nested inline and name mangling
//
// Notes: One potentital performance issue is the overhead of default copy constructor of CALLER_HTABLE.
//        This can be avoided by manipuating on its iterator directly, instead of using an object of
//        CALLEE_HTABLE explicitly.
// ------------------------------------------------------------------------------------------------

enum ISP_ATTRIBUTE {INVALID = 0, CALL = 1, INLINE = 2};

typedef struct callee_info
{
  ISP_ATTRIBUTE  attrib;
  int            process_flag;
  void*          nested_inline;
  callee_info()  {attrib = INVALID; process_flag = 0; nested_inline = NULL;}
} CALLEE_INFO;

typedef hash_map<const char*, CALLEE_INFO, hash<const char*>, eqstr> CALLEE_HTABLE;

typedef hash_map<const char*, CALLEE_HTABLE, hash<const char*>, eqstr> CALLER_HTABLE;

extern void ISP_Fix_Filename(char *);

extern BOOL Check_Inline_Script(const char *, const char *, const char *, MEM_POOL *);

extern void Verify_Inline_Script(void);

extern const char *inline_action_log;
extern const char *inline_script_log;

#endif

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

//-*-c++-*-
//=============================================================================
//
//  Module : gen_base.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/gen_base.h,v $
//
//  Description:
//  ============
//  Aim : some general generate function
//=============================================================================
/////////////////////////////////////////
//
//  ENUM GEN_MODE
//  
//  To indicate Generation mode for easy debugging and reuse.
//
//
#ifndef GEN_BASE_INCLUDED
#define GEN_BASE_INCLUDED

#include "ekapi_ia64.h"

typedef enum {
    GEN_MODE_DEBUG,
    GEN_MODE_FILE,
    GEN_MODE_SCREEN
}GEN_MODE;

typedef enum {
    ITM_TYPE,
    MCK_TYPE
}MACHINE_TYPE;

/**********************************************************
 * Emit_Header
 * Write the common header of a header file
 *   hfile: FILE pointer to the opened header file to write.
 *   name:  name of the module
 *   interface_desc: comment on the description of the
 *                   interface provided in this header
 *                   file.
 */
extern void Emit_Header (FILE *hfile,
		  const char *name,
		  const char * const *interface_desc,
                  bool  cplusplus = false);

/**********************************************************
 * Emit_Tailer
 * Write the common header of a header file
 *   hfile: FILE pointer to the opened header file to write.
 */
extern void Emit_Tailer(FILE *hfile, bool cplusplus = false);

/**********************************************************
 * Init_Module_Files
 * Creat the curresponding .c (.cxx), .h, .Exported file
 * to write
 */
 
extern void Init_Module_Files(GEN_MODE mode, const char * module_name,
         FILE **c_file, FILE **h_file, FILE **export_file,
         bool cplusplus = false);
         
// only write to C file;         
extern void Init_Module_Files(GEN_MODE mode, const char * module_name,
         FILE **c_file, bool cplusplus = false);

/**********************************************************
 * Close_Module_Files
 * Close the curresponding .c (.cxx), .h, .Exported file
 */
extern void Close_Module_Files(GEN_MODE mode,
         FILE **c_file, FILE **h_file, FILE **export_file);

inline void Dot2Line(char *buf)
{
    for (; *buf; buf++){
      if (*buf == '.'){
        *buf='_';
      }
    }
}
inline void Line2Dot(char *buf)
{
    if (buf==NULL) return;
    for (; *buf; buf++){
      if (*buf == '_'){
        *buf='.';
      }
    }
}

#endif /* INCLUDED */


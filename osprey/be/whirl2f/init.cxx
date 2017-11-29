/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


// This file contains only Linux-specific code and should be entirely
// #ifdef'd out for Irix.

// Work around the "undefined weak symbol" bug in Linux.
//
// see comments in be/com/weak.cxx.
//
// This file define initialization of pointer variables to symbols defined
// in lno.so but referenced in be/be.so.

#ifdef __linux__

#include "defs.h"
#include "wn.h"
#include "pu_info.h"
#include "w2f_driver.h"

extern void (*W2F_Cleanup_p)(void);
// extern void (*W2F_Def_ST_p)(FILE *outfile, ST *st);
extern void (*W2F_Fini_p)(void);
extern void (*W2F_Init_p)(void);
extern const char * (*W2F_Object_Name_p)(ST *func_st);
extern void (*W2F_Outfile_Fini_p)(void);
extern void (*W2F_Outfile_Init_p)(void);
extern void (*W2F_Outfile_Translate_Pu_p)(WN *pu);
extern void (*W2F_Pop_PU_p)(void);
extern void (*W2F_Process_Command_Line_p)(INT phase_argc, const char *phase_argv[], 
  INT argc, const char *argv[]);
extern void (*W2F_Push_PU_p)(WN *pu, WN *body_part_of_interest);
extern void (*W2F_Set_Frequency_Map_p)(WN_MAP frequency_map);
extern const char * (*W2F_Get_Transformed_Src_Path_p)(void);
extern BOOL (*W2F_Should_Emit_Nested_PUs_p)(void);
extern void (*W2F_Translate_Istore_Lhs_p)(char *strbuf, UINT bufsize,
  WN* lhs, mINT64 istore_ofst, TY_IDX istore_addr_ty, TYPE_ID istore_mtype);
extern void (*W2F_Translate_Stid_Lhs_p)(char *strbuf, UINT bufsize, 
  ST *stid_st, mINT64 stid_ofst, TY_IDX stid_ty, TYPE_ID stid_mtype);
extern void (*W2F_Translate_Wn_p)(FILE *outfile, WN *wn);
extern void (*W2F_Translate_Wn_Str_p)(char *strbuf, UINT bufsize, WN *wn);

struct W2F_INIT
{
    W2F_INIT () {
      W2F_Cleanup_p = W2F_Cleanup;
      // W2F_Def_ST_p = W2F_Def_ST;
      W2F_Fini_p = W2F_Fini;
      W2F_Init_p = W2F_Init;
      W2F_Object_Name_p = W2F_Object_Name;
      W2F_Outfile_Fini_p = W2F_Outfile_Fini;
      W2F_Outfile_Init_p = W2F_Outfile_Init;
      W2F_Outfile_Translate_Pu_p = W2F_Outfile_Translate_Pu;
      W2F_Pop_PU_p = W2F_Pop_PU;
      W2F_Process_Command_Line_p = W2F_Process_Command_Line;
      W2F_Push_PU_p = W2F_Push_PU;
      W2F_Set_Frequency_Map_p = W2F_Set_Frequency_Map;
      W2F_Get_Transformed_Src_Path_p = W2F_Get_Transformed_Src_Path;
      W2F_Should_Emit_Nested_PUs_p = W2F_Should_Emit_Nested_PUs;
      W2F_Translate_Istore_Lhs_p = W2F_Translate_Istore_Lhs;
      W2F_Translate_Stid_Lhs_p = W2F_Translate_Stid_Lhs;
      W2F_Translate_Wn_p = W2F_Translate_Wn;
      W2F_Translate_Wn_Str_p = W2F_Translate_Wn_Str;
    }
} W2F_Initializer;

#endif // __linux__

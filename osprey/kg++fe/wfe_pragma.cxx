/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
 */

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
#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

#include "gnu_config.h"
#include "gnu/system.h"

#include "srcpos.h"
#include "gnu/flags.h"
#include "gnu/tree.h"
#include "insn-config.h"        // MAX_RECOG_OPERANDS
#include "ir_reader.h"
#include "tree_symtab.h"
#include "wfe_misc.h"
#include "wfe_expr.h"
#include "targ_sim.h"

#ifdef KEY
#include "const.h"
extern "C" void check_gnu_errors (INT *, INT *);
#endif

#include <ctype.h>

#include "wfe_pragma.h"

#ifdef KEY
// This function is intended to be a general function to handle different
// pragmas (other than openmp pragmas). Currently it handles
// #pragma options, mips_frequency_hint
//
extern "C" {

void
WFE_Expand_Pragma (tree exp)
{
  switch (exp->omp.choice)
  {
    case options_dir:
    { // pragma options
      TCON tcon;
      exp = (tree) exp->omp.omp_clause_list;
      tcon = Host_To_Targ_String (MTYPE_STRING,
                                  TREE_STRING_POINTER(exp),
                                  TREE_STRING_LENGTH(exp) - 1 /*ignore \0*/);
      TY_IDX ty_idx = Get_TY(TREE_TYPE(exp));
      ST * st = New_Const_Sym (Enter_tcon (tcon), ty_idx);  
      TREE_STRING_ST (exp) = st;
      WN * wn = WN_CreatePragma (WN_PRAGMA_OPTIONS, st, 0, 0);
      WN * func_wn = WFE_Find_Stmt_In_Stack (wfe_stmk_func_entry);
      WN_INSERT_BlockLast (WN_func_pragmas(func_wn), wn);
      break;
    }
    case exec_freq_dir:
    { // pragma mips_frequency_hint
      MIPS_FREQUENCY_HINT freq_hint;
      Is_True (TREE_CODE ((tree) exp->omp.omp_clause_list) == STRING_CST,
               ("Expected string constant with mips_frequency_hint"));
      const char * hint = TREE_STRING_POINTER ((tree) exp->omp.omp_clause_list);

      if (!strcmp (hint, "never")) freq_hint = FREQUENCY_HINT_NEVER;
      else if (!strcmp (hint, "init")) freq_hint = FREQUENCY_HINT_INIT;
      else if (!strcmp (hint, "frequent")) freq_hint = FREQUENCY_HINT_FREQUENT;
      else // Invalid mips_frequency_hint
        break;

      WN * wn = WN_CreatePragma (WN_PRAGMA_MIPS_FREQUENCY_HINT, (ST*)NULL, freq_hint, 0);
      WFE_Stmt_Append (wn, Get_Srcpos());
      break;
    }
  }
}

void
WFE_Expand_Freq_Hint (tree exp) {

   tree hint_opnd = TREE_OPERAND(exp, 0);
   Is_True (TREE_CODE (hint_opnd) == STRING_CST, 
            ("the first operand of FREQ_HINT_STMT should be STRING_CST"));

   MIPS_FREQUENCY_HINT hint_id;
   char* const hint_name = TREE_STRING_POINTER (hint_opnd);

   if (!strcmp (hint_name, "never")) {
     hint_id = FREQUENCY_HINT_NEVER;
   } else if (!strcmp (hint_name, "init")) {
     hint_id = FREQUENCY_HINT_INIT;
   } else if (!strcmp (hint_name, "frequent")) {
     hint_id = FREQUENCY_HINT_FREQUENT;
   } else {
     Is_True (FALSE, ("unrecognized frequency hint '%s'", hint_name));
   }

   WN* wn = WN_CreatePragma (WN_PRAGMA_MIPS_FREQUENCY_HINT, (ST*)NULL, hint_id, 0);
   WFE_Stmt_Append (wn, Get_Srcpos());
}
}
#endif

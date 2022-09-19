//-*-c++-*-

/*

  Copyright (C) 2018 Xcalibyte (Shenzhen) Limited.

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

 */

/* ====================================================================
 * ====================================================================
 *
 * Module: ir_verify.cxx
 *
 * Revision history:
 *  15-Nov-18 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */
#include "ir_verify.h"
#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include "json/reader.h"

using namespace std;

#define ERROR_PREFIX "WN_verifier Error:"


struct VarLine{
  string varName;
  int lineNumber;
  VarLine(char* name, INT64 line) {
    varName.assign(name);
    lineNumber = line;
  }
};
static multimap<string, int> var_lines;
static int currLine = -1;
static bool skip_func = false;

static BOOL STID_check_type(WN *wn) {
  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  if (opr == OPR_STID) {
    TYPE_ID desc_mtype = WN_desc(wn);
    TY_IDX wn_type = WN_ty(wn);

    if (Is_Structure_Type(wn_type) &&
      (WN_field_id(wn) != 0)) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(wn_type,
                                        WN_field_id(wn),
                                        cur_field_id);
      if (!fld.Is_Null()) {
        wn_type = FLD_type(fld);
      }
    }

    TYPE_ID wn_mtype = TY_mtype(Ty_Table[wn_type]);
    if(wn_mtype != desc_mtype) {
      Fail_FmtAssertion("%s (STID_check_type): STID whirl mtype [%s] != wn desc mtype[%s]", ERROR_PREFIX,  Mtype_Name(wn_mtype), Mtype_Name(desc_mtype));
      return false;
    }
  }
  return true;
}

void WN_verify(WN *wn, WN *parent)
{
  if (wn)
  {
    OPERATOR opr = WN_operator(wn);
    USRCPOS srcpos;
    USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
    int line = SRCPOS_linenum(srcpos);
    if(line != 0) {
      currLine = line;
    }

    if(opr == OPR_FUNC_ENTRY)
    {
      if(line <= 0) {
        skip_func = true;
      } else {
        skip_func = false;
      }
    }
    if(OPERATOR_is_stmt(opr) && currLine <= 0 && !skip_func) {
      // fprintf (stderr, "%s no line number for WHIRL tree with op: %s\n", ERROR_PREFIX, OPCODE_name(WN_opcode(wn)));
    }

    if(!skip_func && WN_has_sym(wn) && WN_operator(wn) != OPR_IDNAME) {
      ST *st = WN_st(wn);
      if(st != NULL && ST_sym_class(st) == CLASS_VAR && !ST_is_temp_var(st) &&
      (ST_sclass(st) == SCLASS_AUTO || ST_sclass(st) == SCLASS_FORMAL) && strncmp(ST_name(st), "__", 2 ) != 0) {
        var_lines.insert(pair<string, int>(ST_name(st), currLine));
      }
    }

    OPCODE op = WN_opcode(wn);
 
    switch(OPCODE_operator(op))
    {
      case OPR_STID:
      {
        STID_check_type(wn);
      }
	  break;
      default:
      break;
    }
    // traverse the tree starting with this node.
    if (op == OPC_BLOCK)
    {
      for(WN *node = WN_first(wn); node; node = WN_next(node))
        WN_verify(node,wn);
    } else {
      if(WN_opcode(wn) == OPC_REGION) {
        WN_verify (WN_region_body(wn), wn);
      } else {
        for(INT32 i = 0; i < WN_kid_count(wn); i++)
          WN_verify(WN_kid(wn,i),wn);
      }
    }
  }
}

bool read_master_file(char *input_file, multimap<string, int> *master_out) {
  char *file_prefix = strtok(input_file,".");
  string master_file_name = "decompile/";
  master_file_name.append( file_prefix );
  master_file_name.append( ".map" );

  ifstream is;
  is.open (master_file_name.c_str());
  if ( (is.rdstate() & ifstream::failbit ) != 0 ) {
    return false;
  }
  Json::Reader reader;
  Json::Value root;
  if (!reader.parse(is, root, false))
  {
    return false;
  }
  string varName;
  int line;
  int size = root.size();
  for (int i=0; i<size; ++i)
  {
    varName = root[i][0].asString();
    line = root[i][1].asInt();;
    master_out->insert(pair<string, int>(varName, line));
  }
  return true;
}

void WN_verify_varName_line(char * input_file) {
  multimap<string, int> master_out;
  bool read_success = read_master_file(input_file, &master_out);

  if(read_success) {
    printf("====================\n");
    for(multimap<string, int>::iterator iter = master_out.begin(); iter != master_out.end(); iter++) {
      printf("  %s->%d\n", iter->first.c_str(), iter->second);
    }

    printf("====================\n");
    for(multimap<string, int>::iterator iter = var_lines.begin(); iter != var_lines.end(); iter++) {
      printf("  %s->%d\n", iter->first.c_str(), iter->second);
      string varName = iter->first;
      int line = iter->second;
      if(line < 0) {
        continue;
      }
      pair <multimap<string,int>::iterator, multimap<string,int>::iterator> ret;
      ret = master_out.equal_range(varName);
      bool found = false;
      for (multimap<string,int>::iterator it=ret.first; it!=ret.second; ++it) {
        int master_line = it->second;
        if(line == master_line) {
          found = true;
          break;
        }
      }
      if(!found) {
        fprintf(stderr, "%s (Veirfy Var Line): unable to find variable %s at line %d\n", ERROR_PREFIX,  varName.c_str(), line);
      }
    }
  }
}


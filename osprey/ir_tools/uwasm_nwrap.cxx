/*
  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

#include <cstdlib>
#include <iostream>
#include <fstream>

#include <errno.h>		    /* for sys_errlist */
#include <stdio.h>		    /* for stderr */
#include <libgen.h>		    /* for basename() */
#include <sys/stat.h>
#include <set>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "mempool.h"
#include "wn.h"			    /* for ir_reader.h */
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"		    /* for IR_reader_init(), etc. */
#include "ir_bwrite.h"		    /* for WN_open_output(), etc. */
#include "ir_bread.h"		    /* for WN_open_input(), etc. */
#include "dwarf_DST_dump.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"              /* for Last_Dedicated_Preg_Offset */

#define REG_KIND 4

BOOL Run_vsaopt = FALSE; // hack to workaround undefine since
BOOL Is_CPP = FALSE;
static FILE *defs_fp;
static FILE *impl_fp;
static std::set<TY_IDX> forward_ty_list;

struct Check_Func_Sym {
  void operator() (UINT idx, ST *st) const;
};

static const char*
TY_to_str(TY_IDX ty, char* buf, INT buf_len) {
  if (TY_kind(ty) == KIND_POINTER) {
    TY_IDX pty = TY_pointed(ty);
    if (TY_kind(pty) == KIND_VOID) {
      strncpy(buf, "void *", buf_len);
    }
    else {
      const char* ret = TY_to_str(TY_pointed(ty), buf, buf_len);
      Is_True(strlen(ret) + 2 < buf_len, ("buffer overflow"));
      strcat(buf, "*");
    }
  }
  else if (TY_kind(ty) == KIND_ARRAY) {
    const char* ret = TY_to_str(TY_etype(ty), buf, buf_len);
    Is_True(strlen(ret) + 2 < buf_len, ("buffer overflow"));
    strcat(buf, "*");
  }
  else if (TY_kind(ty) == KIND_STRUCT) {
    if (TY_is_union(ty)) {
      snprintf(buf, buf_len, "union %s ", TY_name(ty));
    }
    else if (Is_CPP) {
      strncpy(buf, TY_name(ty), buf_len);
    }
    else {
      snprintf(buf, buf_len, "struct %s ", TY_name(ty));
    }
  }
  else if (TY_kind(ty) == KIND_SCALAR) {
    const char* tyn = "";
    switch (TY_mtype(ty)) {
    case MTYPE_I1: tyn = "INT8";   break;
    case MTYPE_U1: tyn = "UINT8";  break;
    case MTYPE_I2: tyn = "INT16";  break;
    case MTYPE_U2: tyn = "UINT16"; break;
    case MTYPE_I4: tyn = "INT32";  break;
    case MTYPE_U4: tyn = "UINT32"; break;
    case MTYPE_I8: tyn = "INT64";  break;
    case MTYPE_U8: tyn = "UINT64"; break;
    case MTYPE_F4: tyn = "float";  break;
    case MTYPE_F8: tyn = "double"; break;
    default: FmtAssert(FALSE, ("bad mtype"));
    }
    strncpy(buf, tyn, buf_len);
  }
  else {
    FmtAssert(FALSE, ("bad ty kind %d xx", TY_kind(ty)));
  }
  return buf;
}

// =============================================================================
// split name_str with namespace
// Convert tfjs::wasm::AA ->
//   namespace tfjs {
//     namespace wasm {
//       struct AA;
//     }
//   }
// =============================================================================
static const char *
Namespace_to_str(const char* name_str, char* buf, INT buf_len) {
  char *double_colon = strstr((char*)name_str, "::");
  char *ret_str = buf;
  vector<const char *> names;
  while(double_colon && *double_colon != '\0') {
    *double_colon = '\0';
    double_colon +=2;
    names.push_back(name_str);
    name_str = double_colon;
    double_colon = strstr(double_colon, "::");
  }
  int idx = 0;
  int len = 0;
  for(; idx < names.size(); idx++) {
    snprintf(buf, buf_len, "namespace %s {\n", names[idx]);
    len = strlen(buf);
    buf +=len;
    buf_len -= len;
  }
  snprintf(buf, buf_len, "  struct %s;\n", name_str);
  len = strlen(buf);
  buf += len;
  buf_len -= len;
  for(idx = 0; idx < names.size(); idx++) {
    snprintf(buf, buf_len, "}\n");
    len = strlen(buf);
    buf += len;
    buf_len -= len;
  }
  Is_True(buf_len > 1, ("Buffer overflow"));
  return ret_str;
}

static const char *
MTYPE_to_reg(TYPE_ID mtype) {
  switch (mtype) {
  case MTYPE_I1:
  case MTYPE_U1:
  case MTYPE_I2:
  case MTYPE_U2:
  case MTYPE_I4:
  case MTYPE_U4: return "i32";
  case MTYPE_I8:
  case MTYPE_U8: return "i64";
  case MTYPE_F4: return "f32";
  case MTYPE_F8: return "f64";
  default: FmtAssert(FALSE, ("bad mtype"));
  }
}

static const char *
MTYPE_to_type(TYPE_ID mtype) {
  switch (mtype) {
  case MTYPE_I1:
  case MTYPE_U1:
  case MTYPE_I2:
  case MTYPE_U2:
  case MTYPE_I4:
  case MTYPE_U4: return "INT32";
  case MTYPE_I8:
  case MTYPE_U8: return "INT64";
  case MTYPE_F4: return "float";
  case MTYPE_F8: return "double";
  case MTYPE_V:  return "void";
  default: FmtAssert(FALSE, ("bad mtype"));
  }
}

static int
reg_index(TYPE_ID mtype) {
  switch (mtype) {
  case MTYPE_I1:
  case MTYPE_U1:
  case MTYPE_I2:
  case MTYPE_U2:
  case MTYPE_I4:
  case MTYPE_U4: return 0;
  case MTYPE_I8:
  case MTYPE_U8: return 1;
  case MTYPE_F4: return 2;
  case MTYPE_F8: return 3;
  default: FmtAssert(FALSE, ("bad mtype"));
  }
  return -1;
}

static const char*
reg_index_str(int idx) {
  Is_True(idx >= 0 && idx <= 3, ("index out of bound"));
  const char* str[] = { "i32", "i64", "f32", "f64" };
  return str[idx];
}

void
Check_Func_Sym::operator() (UINT idx, ST *st) const {
  if (ST_class(st) != CLASS_FUNC)
    return;
  if (ST_sclass(st) != SCLASS_EXTERN)
    return;

  const char *fname = ST_name(st);
  if (strncmp(fname, "_Z", 2) == 0 &&
      strstr(fname, "_impl") == NULL) {
    fprintf(stderr,
            "WARN: ignore %s, function is not uwasm_impl.\n",
            fname);
    return;
  }

  TY_IDX func_ty = ST_type(st);
  if (TY_is_varargs(func_ty)) {
    fprintf(stderr,
            "WARN: ignore %s, function is var args.\n",
            fname);
    return;
  }

  TYLIST_IDX ty_list_idx = TY_tylist(ST_type(st));
  Is_True(ty_list_idx != 0, ("no tylist for function"));

  char ty_buf[64];
  INT reg_used[REG_KIND] = { 0, 0, 0, 0 };
  // return type
  TY_IDX ret_ty = Tylist_Table[ty_list_idx];
  if (TY_kind(ret_ty) == KIND_STRUCT) {
    fprintf(stderr,
            "WARN: ignore %s, return type %s is struct.\n",
            fname, TY_name(ret_ty));
    return;
  } else if((TY_kind(ret_ty) == KIND_POINTER) &&
            (TY_kind(TY_pointed(ret_ty)) == KIND_STRUCT) &&
            forward_ty_list.find(ret_ty) == forward_ty_list.end()) {
    forward_ty_list.insert(ret_ty);
    char namespace_buf[128];
    Namespace_to_str(TY_to_str(TY_pointed(ret_ty), ty_buf, sizeof(ty_buf)),
                     namespace_buf, sizeof(namespace_buf));
    fprintf(impl_fp,
            "%s\n", namespace_buf);
  }
  ++ ty_list_idx;
  if (TY_mtype(ret_ty) != MTYPE_V) {
    ++ reg_used[reg_index(TY_mtype(ret_ty))];
  }

  TYLIST_IDX parm_list_idx = ty_list_idx;

  INT parm_idx = 0;
  TY_IDX parm_ty = Tylist_Table[ty_list_idx];
  while (parm_ty != TY_IDX_ZERO) {
    if (TY_kind(parm_ty) == KIND_STRUCT) {
      fprintf(stderr,
              "WARN: ignore %s, param %d type %s is struct.\n",
              fname, parm_idx, TY_name(ret_ty));
      return;
    }
    ++ reg_used[reg_index(TY_mtype(parm_ty))];
    ++ parm_idx;
    ++ ty_list_idx;
    parm_ty = Tylist_Table[ty_list_idx];
  }

  fprintf(defs_fp,
          "DEFINE_SYSCALL(%s, ", fname);
  fprintf(defs_fp,
          "%s",
          MTYPE_to_type(TY_mtype(ret_ty)));

  fprintf(impl_fp,
          "// native call wrapper for %s\n", fname);
  fprintf(impl_fp,
          "extern \"C\" %s %s(",
          TY_kind(ret_ty) == KIND_VOID ?
              "void" : TY_to_str(ret_ty, ty_buf, sizeof(ty_buf)),
          fname);
  ty_list_idx = parm_list_idx;
  parm_ty = Tylist_Table[ty_list_idx];
  while (parm_ty != TY_IDX_ZERO) {
    if (ty_list_idx != parm_list_idx) {
      fprintf(impl_fp, ", ");
    }
    fprintf(impl_fp,
            "%s",
            TY_to_str(parm_ty, ty_buf, sizeof(ty_buf)));
    ++ ty_list_idx;
    parm_ty = Tylist_Table[ty_list_idx];
  }
  fprintf(impl_fp, ");\n");

  fprintf(impl_fp,
          "void UWASM_SYSTEM::VM_SYSCALL_%s(UWASM_SYSTEM *self) {\n", fname);
  if (TY_mtype(ret_ty) != MTYPE_V || parm_idx > 0) {
    fprintf(impl_fp,
            "  // get output param location\n");
  }

  for (int i = 0; i < REG_KIND; ++i) {
    if (reg_used[i] > 0) {
      const char *reg_kind_s[] = { "I32", "I64", "F32", "F64" };
      fprintf(impl_fp,
              "  UINT32 parm_loc_%s = self->Machine().Cur_env().Get_fp(%s) + RET_REGITER_SIZE + \n",
              reg_index_str(i), reg_kind_s[i]);
      fprintf(impl_fp,
              "                       self->Machine().Get_local_count_%s(self->Get_cur_func_idx()) +\n",
              reg_index_str(i));
      fprintf(impl_fp,
              "                       self->Machine().Get_u_module().Get_func_param_num<%s>(self->Get_cur_func_idx());\n",
              reg_kind_s[i]);
    }
  }

  if (parm_idx > 0) {
    fprintf(impl_fp,
            "\n  // get output param from register stack\n");
  }

  // parameters
  INT reg_count[REG_KIND] = { 0, 0, 0, 0 };
  ty_list_idx = parm_list_idx;
  parm_ty = Tylist_Table[ty_list_idx];
  parm_idx = 0;
  while (parm_ty != TY_IDX_ZERO) {
    const char* ty_name = TY_to_str(parm_ty, ty_buf, sizeof(ty_buf));
    const char* reg_name = MTYPE_to_reg(TY_mtype(parm_ty));
    int idx = reg_index(TY_mtype(parm_ty));
    if(TY_kind(parm_ty) == KIND_POINTER) {
      fprintf(impl_fp,
              "  INT32 ofst_%d = (INT32)self->Machine().Get_reg_%s((REG_INDEX)"
              "(parm_loc_%s + RET_REGITER_SIZE + %d));\n",
              parm_idx, reg_name, reg_name, reg_count[idx]++);
      fprintf(impl_fp,
              "  Is_True(ofst_%d >= 0 && (UINT32)ofst_%d < self->Machine().Get_mem_size(), "
              "(\"function %s: memory offset outof range\"));\n",
              parm_idx, parm_idx, fname);
      fprintf(impl_fp, "  %s parm_%d;\n", ty_name, parm_idx);
      fprintf(impl_fp, "  if(ofst_%d > 0) {\n", parm_idx);
      fprintf(impl_fp,
              "    parm_%d = (%s)((uintptr_t)self->Machine().Get_mem_ptr() + ofst_%d);\n",
               parm_idx, ty_name, parm_idx);
      fprintf(impl_fp, "  } else {\n    parm_%d = (%s)0; \n  }\n",
              parm_idx, ty_name);
      parm_idx++;

    } else {
      fprintf(impl_fp,
              "  %s parm_%d = (%s)self->Machine().Get_reg_%s((REG_INDEX)"
              "(parm_loc_%s + RET_REGITER_SIZE + %d));\n",
              ty_name, parm_idx ++, ty_name,
              reg_name, reg_name,
              reg_count[idx] ++
              );
    }
    fprintf(defs_fp,
            ", %s",
            MTYPE_to_type(TY_mtype(parm_ty)));

    ++ ty_list_idx;
    parm_ty = Tylist_Table[ty_list_idx];
  }
  fprintf(defs_fp, ")\n");

  if (parm_idx > 0)
    fprintf(impl_fp, "\n");
  fprintf(impl_fp,
          "  // call function %s\n", fname);

  if (TY_mtype(ret_ty) != MTYPE_V) {
    const char *ty_name = TY_to_str(ret_ty, ty_buf, sizeof(ty_buf));
    if(TY_kind(ret_ty) == KIND_POINTER) {
      fprintf(impl_fp,
              "  INT32 retv = (%s)((uintptr_t)%s(",
              MTYPE_to_type(TY_mtype(ret_ty)), ST_name(st));
    } else {
      fprintf(impl_fp,
              "  %s retv = (%s)%s(",
              ty_name, ty_name, ST_name(st));
    }
  }
  else {
    fprintf(impl_fp,
            "  %s(",
            ST_name(st));
  }

  for (INT i = 0; i < parm_idx; ++i) {
    if (i > 0)
      fprintf(impl_fp, ", ");
    fprintf(impl_fp, "parm_%d", i);
  }
  if(TY_kind(ret_ty) == KIND_POINTER) {
    fprintf(impl_fp, ") - (uintptr_t)self->Machine().Get_mem_ptr());\n");
    fprintf(impl_fp, "  Is_True(retv >= 0 && (UINT32)retv < self->Machine().Get_mem_size(),"
            "\n          (\"function %s: memory offset outof range\"));\n", fname);
  } else {
    fprintf(impl_fp, ");\n");
  }

  if (TY_mtype(ret_ty) != MTYPE_V) {
    fprintf(impl_fp,
            "\n  // store return value to stack\n");
    fprintf(impl_fp,
            "  self->Machine().Set_reg_%s((REG_INDEX) parm_loc_%s, (%s)retv);\n",
            MTYPE_to_reg(TY_mtype(ret_ty)),
            MTYPE_to_reg(TY_mtype(ret_ty)),
            MTYPE_to_type(TY_mtype(ret_ty)));
  }
  fprintf(impl_fp, "}\n\n");
}

void
init_files(const char *in_file) {
  char *pos = strrchr((char*)in_file, '.');
  size_t len = pos ? pos - in_file : strlen(in_file);
  char *buf = (char*)malloc(len + 16);
  strncpy(buf, in_file, len);

  // defs file
  strcpy(buf + len, ".nwrap.def");
  printf("Write declaration to %s\n", buf);
  defs_fp = fopen(buf, "w");
  Is_True(defs_fp, ("Failed to open definition file %s", buf));

  // impl file
  strcpy(buf + len, ".nwrap.cxx");
  printf("Write implementation to %s\n", buf);
  impl_fp = fopen(buf, "w");
  Is_True(impl_fp, ("Failed to open implementation file %s", buf));
}

void
close_files() {
  if (defs_fp)
    fclose(defs_fp);
  if (impl_fp)
    fclose(impl_fp);
}

static void
ir_b2a (char *global_file, char *input_file) {
    PU_Info *pu_tree;

    if (global_file == NULL) {
	(void)Open_Input_Info (input_file);
    } else {
        (void)Open_Global_Input (global_file);
        (void)Open_Local_Input (input_file);
    }

    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
    pu_tree = Read_Global_Info (NULL);

    IR_reader_init();
    IR_Dwarf_Gen_File_Table(FALSE);  // this will set srcFileTable

    // set the language based on the first PU
    if (pu_tree != NULL) {
      PU_Info *pu = pu_tree;
      Read_Local_Info (MEM_pu_nz_pool_ptr, pu);
      WN *wn = PU_Info_tree_ptr(pu);
      ST_IDX func_st_idx = WN_st_idx(wn);
      ST *func_st = &St_Table[func_st_idx];
      if (Pu_Table[func_st->u2.pu].src_lang & PU_CXX_LANG) {
        Is_CPP = TRUE;
      } else if (Pu_Table[func_st->u2.pu].src_lang & PU_C_LANG) {
      }
    }

#if 0
    for (INT idx = 0; idx < srcFileTable.size(); idx++) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(srcFileTable[idx]);
      theModule.srcFileInfo.push_back(MIRInfoPair(strIdx, idx+1));
    }

    MIRBuilder mirBuilder(&theModule);
    WHIRL2mpl whirl2mpl(&mirBuilder);

    // translate all the types
    For_all (Ty_Table, TY2mpl(&whirl2mpl));
    if (whirl2mpl.hasForwardTypeReference) {
      For_all (Ty_Table, FixForwardTypeRef(&whirl2mpl));
    }

    // translate global symbols
    For_all (St_Table, GLOBAL_SYMTAB, ST2mpl(&whirl2mpl));

    // translate the function bodies
    ir_b2a_process_PUs(pu_tree, &whirl2mpl);

    // translate the initializations
    For_all (Inito_Table, GLOBAL_SYMTAB, INITO2mpl(&whirl2mpl));

    // process the global ST_ATTR_TAB
    For_all (St_Attr_Table, GLOBAL_SYMTAB, ST_ATTR2mpl(&whirl2mpl));

    /* print the symbol tables */
//  Print_global_symtab (stdout);
#endif
    forward_ty_list.clear();

    init_files (input_file);

    For_all (St_Table, GLOBAL_SYMTAB, Check_Func_Sym());

    Free_Input_Info ();
    close_files ();

#if 0
    // output maple file
    theModule.flavor = kFeProduced;
    theModule.numFuncs = theModule.functionList.size();
    if (!use_binary) {
      theModule.OutputAsciiMpl("", ".mpl");
    } else {
      BinaryMplt binMplt(theModule);
      std::string modname = theModule.fileName;
      std::string::size_type lastdot = modname.find_last_of(".");
      std::string filestem = modname.substr(0, lastdot);
      binMplt.GetBinExport().not2mplt = TRUE;
      binMplt.Export(filestem + ".bpl");
    }
#endif
} // ir_b2a

extern BOOL
file_exists (char *path)
{
        INT st;
        struct stat sbuf;
        st = stat(path, &sbuf);
        if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
                return FALSE;
        else
                return TRUE;
}

static void
usage (char *progname)
{
  fprintf (stderr, "Usage: %s [-a] <Whirl IR>\n", progname);
  exit (1);
}

int main (INT argc, char *argv[])
{
    register char *progname;
    register INT a2b, b2a, sel, all;
    char *infile;

    MEM_Initialize();
    Set_Error_Tables (Phases, host_errlist);
    Init_Error_Handler (10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    WHIRL_Mldid_Mstid_On = TRUE;
    Pointer_Size = 8;  // this is because Configure_Target() was not called

    progname = basename (argv[0]);
    // weird linux bug with basename where it doesn't strip the leading /
    if (*progname == '/') ++progname;

    Read_Global_Data = NULL;
    if (argc < 2)
      usage(progname);
    INT binarg = 1;
#if 0
    if (*argv[binarg] == '-') {
      if (strncmp(argv[binarg], "-a", 2) == 0) {
        use_binary = FALSE;
      } else {
        usage(progname);
      }
      binarg++;
    }
#endif
    if (binarg == argc)
        usage(progname);
    if (!file_exists(argv[binarg]))
        usage(progname);
    infile = argv[binarg];
    ir_b2a (Read_Global_Data, infile);

    exit (0);
} /* main */


/* Dummy definitions to satisify references from routines that got pulled
 * in by the header files but are never called
 */
void
Signal_Cleanup (INT sig) { }

const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    fprintf (stderr, "Internal: Host_Format_Parm () not implemented\n");
    return "";
}

INT8 Debug_Level = 0;

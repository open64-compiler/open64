/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
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


#include "uwasm_nat_util.h"
#include "wn.h"
#include "symtab.h"
#include "wn_lower.h"
#include "wn_trap.h"

// UWASM global variable for isolation

// create an external symbol with given name
static ST *
Uwasm_create_st(const char* name)
{
  TY_IDX ty = MTYPE_To_TY(Pointer_Mtype);
  ST* st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str(name), CLASS_VAR,
          SCLASS_UGLOBAL, EXPORT_HIDDEN, ty);
  return st;
}

// get external symbol for address mask
ST *
Uwasm_addr_mask_st()
{
  static ST *addr_mask = NULL;
  if (addr_mask == NULL)
    addr_mask = Uwasm_create_st("__uwasm_addr_mask");
  return addr_mask;
}

// get external symbol for address base
ST *
Uwasm_addr_base_st()
{
  static ST *addr_base = NULL;
  if (addr_base == NULL)
    addr_base = Uwasm_create_st("__uwsam_addr_base");
  return addr_base;
}

// get external symbol for offset mask
ST *
Uwasm_ofst_mask_st()
{
  static ST *ofst_mask = NULL;
  if (ofst_mask == NULL)
    ofst_mask = Uwasm_create_st("__uwasm_ofst_mask");
  return ofst_mask;
}

// no need to check addr if it's LDA or ARRAY of LDA
BOOL
Uwasm_check_addr(WN *addr)
{
  Is_True(addr != NULL, ("null addr"));
  if (WN_operator(addr) == OPR_LDA)
    return FALSE;
  if (WN_operator(addr) == OPR_ARRAY)
    return Uwasm_check_addr(WN_kid0(addr));
  return TRUE;
}


// Instru address in VHO phase
static WN *
Uwasm_instru_address_in_vho(WN *block, WN *addr, SRCPOS spos)
{
  Is_True(Uwasm_Isolation != UWASM_ISO_NONE,
          ("Uwasm ISO is off"));

  if (Uwasm_check_addr(addr) == FALSE)
    return addr;

  if (Uwasm_Early_Expand == FALSE) {
    WN *args[1];
    args[0] = WN_CreateParm(WN_rtype(addr), addr,
                            Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)),
                            WN_PARM_BY_VALUE);
    addr = WN_Create_Intrinsic(OPR_INTRINSIC_OP,
                               Pointer_Mtype, MTYPE_V,
                               INTRN_CHECK_ADDRESS,
                               1, args);
    return addr;
  }

  if (WN_operator(addr) != OPR_LDID &&
      WN_operator(addr) != OPR_INTCONST &&
      WN_operator(addr) != OPR_LDA) {
    // volatile not handled, the code may run different behavior at runtime
    UINT64 ofst = Create_Preg(Pointer_Mtype, ".uwasm.addr");
    TY_IDX ty = Make_Pointer_Type(MTYPE_To_TY(Pointer_Mtype));
    ST *st = MTYPE_To_PREG(Pointer_Mtype);
    WN *stid = WN_Stid(Pointer_Mtype, ofst, st, ty, addr);
    WN_INSERT_BlockLast(block, stid);
    WN_Set_Linenum(stid, spos);
    addr = WN_Ldid(Pointer_Mtype, ofst, st, ty);
  }

  if (Uwasm_Extern_Symbol) {
    if (Uwasm_Isolation == UWASM_ISO_ASSERT) {
      WN *addr_mask = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_mask_st(),
                              MTYPE_To_TY(Pointer_Mtype), 0);
      WN *addr_band = WN_Band(Pointer_Mtype,
                              WN_COPY_Tree(addr), addr_mask);
      WN *addr_base = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_base_st(),
                              MTYPE_To_TY(Pointer_Mtype), 0);
      WN *addr_cmp = WN_EQ(MTYPE_I4, addr_band, addr_base);
      WN *assert = WN_CreateAssert(0, addr_cmp);
      WN_INSERT_BlockLast(block, assert);
      WN_Set_Linenum(assert, spos);
    }
    else if (Uwasm_Isolation == UWASN_ISO_WRAP) {
      WN *ofst_mask = WN_Ldid(Pointer_Mtype, 0, Uwasm_ofst_mask_st(),
                              MTYPE_To_TY(Pointer_Mtype), 0);
      addr = WN_Band(Pointer_Mtype, addr, ofst_mask);
      WN *addr_base = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_base_st(),
                              MTYPE_To_TY(Pointer_Mtype), 0);
      addr = WN_Bior(Pointer_Mtype, addr, addr_base);
    }
  }
  else {
    WN *assert = WN_CreateAssert(WN_TRAP_UWASM_BOUNDS_ERROR,
                                 WN_COPY_Tree(addr));
    WN_INSERT_BlockLast(block, assert);
    WN_Set_Linenum(assert, spos);
  }
  return addr;
}

// Instru load in VHO phase
void
Uwasm_instru_load_in_vho(WN *block, WN *load, SRCPOS spos)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK,
          ("invalid block"));
  Is_True(load && WN_operator(load) == OPR_ILOAD,
          ("invalid iload"));
  WN *addr = Uwasm_instru_address_in_vho(block, WN_kid0(load), spos);
  Is_True(addr != NULL,
          ("invalid addr"));
  WN_kid0(load) = addr;
}

// Instru store in Lower-To-CG phase
void
Uwasm_instru_store_in_vho(WN *block, WN *store, SRCPOS spos)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK,
          ("invalid block"));
  Is_True(store && WN_operator(store) == OPR_ISTORE,
          ("invalid istore"));
  WN *addr = Uwasm_instru_address_in_vho(block, WN_kid1(store), spos);
  Is_True(addr != NULL,
          ("invalid addr"));
  WN_kid1(store) = addr;
}


// Instru address in Lower-To-CG phase
static WN *
Uwasm_instru_address_before_cg(WN *block, WN *addr, SRCPOS spos)
{
  Is_True(Uwasm_Isolation != UWASM_ISO_NONE,
          ("Uwasm ISO is off"));
  if (Uwasm_check_addr(addr) == FALSE)
    return addr;

  if (WN_operator(addr) != OPR_LDID &&
      WN_operator(addr) != OPR_INTCONST &&
      WN_operator(addr) != OPR_LDA) {
    // volatile not handled, the code may run different behavior at runtime
    UINT64 ofst = Create_Preg(Pointer_Mtype, ".uwasm.addr");
    ST *st = MTYPE_To_PREG(Pointer_Mtype);
    WN *stid = WN_Stid(Pointer_Mtype, ofst, st, MTYPE_To_TY(Pointer_Mtype), addr);
    WN_INSERT_BlockLast(block, stid);
    WN_Set_Linenum(stid, spos);
    addr = WN_Ldid(Pointer_Mtype, ofst, st, MTYPE_To_TY(Pointer_Mtype));
  }
  if (Uwasm_Isolation == UWASM_ISO_ASSERT) {
    WN *addr_mask = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_mask_st(),
                            MTYPE_To_TY(Pointer_Mtype), 0);
    WN *addr_band = WN_Band(Pointer_Mtype,
                            WN_COPY_Tree(addr), addr_mask);
    WN *addr_base = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_base_st(),
                            MTYPE_To_TY(Pointer_Mtype), 0);
    WN *addr_cmp = WN_EQ(MTYPE_I4, addr_band, addr_base);
    WN *assert = WN_CreateAssert(0, addr_cmp);
    WN_Set_Linenum(assert, spos);
    assert = WN_lower_assert(block, assert,
                             LOWER_ASSERT | LOWER_IF);
    WN_INSERT_BlockLast(block, assert);
  }
  else if (Uwasm_Isolation == UWASN_ISO_WRAP) {
    WN *ofst_mask = WN_Ldid(Pointer_Mtype, 0, Uwasm_ofst_mask_st(),
                            MTYPE_To_TY(Pointer_Mtype), 0);
    addr = WN_Band(Pointer_Mtype, addr, ofst_mask);
    WN *addr_base = WN_Ldid(Pointer_Mtype, 0, Uwasm_addr_base_st(),
                            MTYPE_To_TY(Pointer_Mtype), 0);
    addr = WN_Bior(Pointer_Mtype, addr, addr_base);
  }
  return addr;
}

// Instru load in Lower-To-CG phase
void
Uwasm_instru_load_before_cg(WN *block, WN *load, SRCPOS spos)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK,
          ("invalid block"));
  Is_True(load && WN_operator(load) == OPR_ILOAD,
          ("invalid iload"));
  WN *addr = Uwasm_instru_address_before_cg(block, WN_kid0(load), spos);
  Is_True(addr != NULL,
          ("invalid addr"));
  WN_kid0(load) = addr;
}

// Instru store in Lower-To-CG phase
void
Uwasm_instru_store_before_cg(WN *block, WN *store, SRCPOS spos)
{
  Is_True(block && WN_operator(block) == OPR_BLOCK,
          ("invalid block"));
  Is_True(store && WN_operator(store) == OPR_ISTORE,
          ("invalid istore"));
  WN *addr = Uwasm_instru_address_before_cg(block, WN_kid1(store), spos);
  Is_True(addr != NULL,
          ("invalid addr"));
  WN_kid1(store) = addr;
}


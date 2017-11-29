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

#include <stdio.h>
#include <stdlib.h>
#include "libelf/libelf.h"
#include <list>
#include "elf_stuff.h"
#include <elfaccess.h>

#include "defs.h"
#include "erglob.h"
#include "glob.h"
#include "flags.h"
#include "tracing.h"
#include "config.h"
#include "config_asm.h"
#include "be_util.h"
#include "cgir.h"
#include "register.h"
#include "tn_map.h"
#include "em_elf.h"
#include "em_dwarf.h"
#include "cgtarget.h"
#include "calls.h"
#include "cgemit.h"
#include "data_layout.h"
#include "cgdwarf_targ.h"
#include <list.h>
#include <sys/unwindP.h>
static BOOL Trace_Unwind = FALSE;

// Procedure regions:
typedef enum
{
    UNDEFINED_UREGION,
    PROLOGUE_UREGION,
    EPILOGUE_BODY_UREGION,
    LABEL_BODY_UREGION,
    COPY_BODY_UREGION
} UREGION_TYPE;

// Unwind elements:
enum
{
    UE_UNDEFINED,
    UE_CREATE_FRAME,
    UE_DESTROY_FRAME,
    UE_SAVE_GR,		// save a reg to a GR reg
    UE_SAVE_SP,		// save a reg to memory (sp)
    UE_SAVE_PSP,	// save a reg to memory (psp)
    UE_RESTORE_GR,	// restore a reg from a GR reg
    UE_RESTORE_MEM,	// restore a reg from memory
    UE_EPILOG,		// body epilog
    UE_LABEL,		// body label
    UE_COPY		// body copy
};

typedef struct unwind_elem
{
    mUINT32 when;
    BB *bb;
    mUINT8 kind;
    mUINT8 qp;			// reg number of qualifying predicate
    mUINT16 label;		// body label id
    CLASS_REG_PAIR rc_reg;	// reg whose state is changing
    CLASS_REG_PAIR save_rc_reg;	// reg being saved into
    union
    {
        mINT64 offset;			// stack offset
        struct
        {
            mUINT32 size;		// region size
            mUINT32 start;		// when at start of region
        } region;
    } u;
} UNWIND_ELEM;

// use list not slist cause append to end
static list < UNWIND_ELEM > ue_list;
static list < UNWIND_ELEM >::iterator ue_iter;
static UINT last_when;
static BOOL simple_unwind = FALSE;
static BOOL has_asm = FALSE;
static UINT last_label = 0;
static BOOL has_create = FALSE;
static TN_MAP tn_def_op;

static const char *
UE_Register_Name(ISA_REGISTER_CLASS rc, REGISTER r)
{
    if (rc == ISA_REGISTER_CLASS_branch && r == REGISTER_MIN + 0)
        return "rp";	// b0 is called rp in unwind directives
    else
        return REGISTER_name(rc, r);
}

void
Init_Unwind_Info(BOOL trace)
{
    return;
}

void
Finalize_Unwind_Info(void)
{
    ue_list.clear();
    return;
}

void
Emit_Unwind_Directives_For_OP(OP *op, FILE *f)
{
    return;
}

static void
Add_Prologue_Header(__unw_info_t *uinfo, UINT64 size)
{
    __unw_error_t st = 0;
    if (Trace_Unwind) fprintf(TFile, "prolog header size %llu\n", size);
    st |= unwind_info_add_prologue_header(uinfo, size);
    Is_True(st == __UNW_OK, ("unwind_info prolog error (%d)", st));
}

static void
Add_Body_Header(__unw_info_t *uinfo, UINT64 size)
{
    __unw_error_t st = 0;
    if (Trace_Unwind) fprintf(TFile, "body header size %llu\n", size);
    st = unwind_info_add_body_header(uinfo, size);
    Is_True(st == __UNW_OK, ("unwind_info body error (%d)", st));
}

// process info we've collected and create the unwind descriptors
static void
Create_Unwind_Descriptors(__unw_info_t *uinfo)
{
    __unw_error_t st = 0;
    ISA_REGISTER_CLASS rc;
    REGISTER reg;
    UREGION_TYPE uregion = UNDEFINED_UREGION;
    UINT start_when = 0;
    UINT end_when = 0;

    if (!simple_unwind) return;	// TODO

    for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter)
    {
        if (ue_iter->kind == UE_CREATE_FRAME)
            break;
    }
    if (ue_iter == ue_list.end())
    {
        // no frame
        Add_Prologue_Header(uinfo, last_when);
    }
    else
    {
        Add_Prologue_Header(uinfo, ue_iter->u.region.size);

        if (Current_PU_Stack_Model == SMODEL_SMALL)
        {
            if (Trace_Unwind) fprintf(TFile, "fixed stack frame of size %lld, set at when %d\n",
                                          Frame_Len, ue_iter->when);
            st |= unwind_info_add_prologue_mem_stack_f_info(uinfo,
                    ue_iter->when, Frame_Len / INST_BYTES);
        }
        else
        {
            if (Trace_Unwind) fprintf(TFile, "large or variable-size stack frame, when = %d\n", ue_iter->when);
            st |= unwind_info_add_prologue_mem_stack_v_info(uinfo, ue_iter->when);
            st |= unwind_info_add_prologue_psp_gr_info(uinfo,
                    REGISTER_machine_id(
                        TN_register_class(FP_TN), TN_register(FP_TN)));
        }
        Is_True(st == __UNW_OK, ("unwind_info mem_stack error (%d)", st));
    }
    uregion = PROLOGUE_UREGION;

    for (ue_iter = ue_list.begin(); ue_iter != ue_list.end(); ++ue_iter)
    {
        switch (ue_iter->kind)
        {
        case UE_EPILOG:
            Add_Body_Header(uinfo, ue_iter->u.region.size);
            uregion = EPILOGUE_BODY_UREGION;
            start_when = ue_iter->u.region.start;
            end_when = start_when + ue_iter->u.region.size;
            break;
        case UE_DESTROY_FRAME:
        {
            INT when_from_end = end_when - ue_iter->when - 1;
            Is_True(end_when != 0, ("unwind: no epilog before destroy_frame?"));
            if (Trace_Unwind) fprintf(TFile, "body epilogue at when %d\n", when_from_end);
            st |= unwind_info_add_body_epilogue_info(uinfo, when_from_end, 0);
            Is_True(st == __UNW_OK, ("unwind_info frame restore error (%d)", st));
        }
        break;
        case UE_LABEL:
            Add_Body_Header(uinfo, ue_iter->u.region.size);
            uregion = LABEL_BODY_UREGION;

            if (Trace_Unwind) fprintf(TFile, "body label at when %d\n", ue_iter->when);
            st |= unwind_info_add_body_label_state_info(uinfo, ue_iter->label);
            Is_True(st == __UNW_OK, ("unwind_info label error (%d)", st));
            start_when = ue_iter->u.region.start;
            break;
        case UE_COPY:
            Add_Body_Header(uinfo, ue_iter->u.region.size);
            uregion = COPY_BODY_UREGION;

            if (Trace_Unwind) fprintf(TFile, "body copy at when %d\n", ue_iter->when);
            st |= unwind_info_add_body_copy_state_info(uinfo, ue_iter->label);
            Is_True(st == __UNW_OK, ("unwind_info copy error (%d)", st));
            start_when = ue_iter->u.region.start;
            break;

        case UE_SAVE_GR:
        {
            rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
            reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
            ISA_REGISTER_CLASS save_rc = CLASS_REG_PAIR_rclass(ue_iter->save_rc_reg);
            REGISTER save_reg = CLASS_REG_PAIR_reg(ue_iter->save_rc_reg);
            if (Trace_Unwind) fprintf(TFile, "save reg to reg at when %d\n", ue_iter->when);

            if (uregion == PROLOGUE_UREGION)
            {
                st |= unwind_info_add_prologue_info_reg(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          save_rc,
                          REGISTER_machine_id(save_rc, save_reg));
            }
            else
            {
                // need to handle saves and restores in copy and label bodies
                st |= unwind_info_add_body_info_reg(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          save_rc,
                          REGISTER_machine_id(save_rc, save_reg));
            }
        }
        Is_True(st == __UNW_OK, ("unwind_info save error (%d) on reg %s",
                                 st, UE_Register_Name(rc, reg)));
        break;

        case UE_SAVE_SP:
            rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
            reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
            if (Trace_Unwind) fprintf(TFile, "save reg to sp mem at when %d\n", ue_iter->when);
            if (uregion == PROLOGUE_UREGION)
            {
                st |= unwind_info_add_prologue_info_sp_offset(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          ue_iter->u.offset);
            }
            else
            {
                st |= unwind_info_add_body_info_sp_offset(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          ue_iter->u.offset);
            }
            Is_True(st == __UNW_OK, ("unwind_info prolog error (%d) on reg %s",
                                     st, UE_Register_Name(rc, reg)));
            break;

        case UE_SAVE_PSP:
            rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
            reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);

            if (Trace_Unwind) fprintf(TFile, "save reg to psp mem at when %d\n", ue_iter->when);
            if (uregion == PROLOGUE_UREGION)
            {
                st |= unwind_info_add_prologue_info_psp_offset(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          ue_iter->u.offset);
            }
            else
            {
                st |= unwind_info_add_body_info_psp_offset(
                          uinfo,
                          rc,
                          REGISTER_machine_id(rc, reg),
                          ue_iter->when - start_when,
                          ue_iter->u.offset);
            }
            Is_True(st == __UNW_OK, ("unwind_info prolog error (%d) on reg %s",
                                     st, UE_Register_Name(rc, reg)));
            break;

        case UE_RESTORE_MEM:
            // can ignore restores in memory, as memory survives
            break;
        case UE_RESTORE_GR:
            // can ignore restores in epilog (all are restored)
            if (uregion == EPILOGUE_BODY_UREGION)
                break;
            rc = CLASS_REG_PAIR_rclass(ue_iter->rc_reg);
            reg = CLASS_REG_PAIR_reg(ue_iter->rc_reg);
            if (Trace_Unwind) fprintf(TFile, "restore reg at when %d\n", ue_iter->when);
            st |= unwind_info_add_body_info_restore(uinfo,
                                                    rc,
                                                    REGISTER_machine_id(rc, reg),
                                                    ue_iter->when - start_when);
            Is_True(st == __UNW_OK, ("unwind_info restore error (%d) on reg %s",
                                     st, UE_Register_Name(rc, reg)));
            break;
        }
    }
}

// dump unwind table and info to .s file
__unw_error_t
unwind_dump2asm(char *unwind_table_ptr,
                __uint64_t unwind_table_size,
                char *unwind_info_ptr,
                __uint64_t unwind_info_size,
                void *arg) // last_pc-first_pc
{
    static __uint64_t last_info_size = 0;
    static __uint64_t last_table_size = 0;
    __uint64_t i;
    __uint64_t unwind_table_size_in_entries =
        unwind_table_size / sizeof(__unw_table_entry_t);
    fprintf(Asm_File, "// emit unwind info\n");
    // gas knows what attributes to give unwind sections
    fprintf(Asm_File, "\t%s %s\n", AS_SECTION, IA64_UNWIND_INFO);
    // dump section in 8-byte chunks
    fprintf(Asm_File, ".Lunwind_info_%d:\n", Current_PU_Count());
    for (i = last_info_size; i < unwind_info_size; i += 8)
    {
        fprintf(Asm_File, "\t%s %#llx\n", AS_DWORD, *(__uint64_t *)(unwind_info_ptr + i));
    }
    fprintf(Asm_File, "\t%s %s\n", AS_SECTION, IA64_UNWIND);
    // should always be 3 double-words
    i = last_table_size;
    fprintf(Asm_File, "\t%s @segrel(%s#)\n", AS_DWORD, Cur_PU_Name);
    i += 8;
    fprintf(Asm_File, "\t%s @segrel(%s#+%#llx)\n", AS_DWORD, Cur_PU_Name,
            (unsigned long)(arg));
    fprintf(Asm_File, "\t%s @segrel(.Lunwind_info_%d)\n", AS_DWORD, Current_PU_Count());
    last_info_size = unwind_info_size;
    last_table_size = unwind_table_size;
}

/* construct the fde for the current procedure. */
extern Dwarf_P_Fde
Build_Fde_For_Proc(Dwarf_P_Debug dw_dbg, BB *firstbb,
                   LABEL_IDX begin_label,
                   LABEL_IDX end_label,
                   INT32     end_offset,
                   // The following two arguments need to go away
                   // once libunwind gives us an interface that
                   // supports symbolic ranges.
                   INT       low_pc,
                   INT       high_pc)
{
    __unw_info_t *uinfo = NULL;
    __unw_error_t st;

    if (! CG_emit_unwind_info) return NULL;
    if (CG_emit_unwind_directives) return NULL;
    // else emit unwind info directly as data

    st = unwind_info_initialize(&uinfo, low_pc, high_pc);
    Is_True(st == __UNW_OK, ("unwind_info initialize error (%d)", st));

    // process info we've collected and create the unwind descriptors
    Create_Unwind_Descriptors(uinfo);

    st = unwind_info_finalize(uinfo);
    FmtAssert(st == __UNW_OK, ("unwind_info finalize error (%d)", st));

    if (has_asm)
        DevWarn("no unwind info cause PU has asm");
    else if (! simple_unwind)
        DevWarn("no unwind info cause PU is too complicated");
    if (simple_unwind)
    {
        unwind_process(unwind_dump2asm, (void*)(high_pc - low_pc));
    }
    return NULL;	// no fde when generate unwind stuff
}

void
Check_Dwarf_Rel(const Elf32_Rel &current_reloc)
{
    FmtAssert(REL32_type(current_reloc) == R_IA_64_DIR32MSB,
              ("Unimplemented 32-bit relocation type %d",
               REL32_type(current_reloc)));
}

void
Check_Dwarf_Rel(const Elf64_AltRel &current_reloc)
{
    FmtAssert(REL64_type(current_reloc) == R_IA_64_DIR64MSB,
              ("Unimplemented 64-bit relocation type %d",
               REL64_type(current_reloc)));
}

void
Check_Dwarf_Rela(const Elf64_AltRela &current_reloc)
{
    FmtAssert(FALSE,
              ("Unimplemented 64-bit relocation type %d",
               REL64_type(current_reloc)));
}

void
Check_Dwarf_Rela(const Elf32_Rela &current_reloc)
{
    FmtAssert(FALSE,
              ("Unimplemented 32-bit relocation type %d",
               REL32_type(current_reloc)));
}

static char *drop_these[] =
{
    // Assembler generates .debug_line from directives itself, so we
    // don't output it.
    ".debug_line",
    0
};

extern BOOL Is_Dwarf_Section_To_Emit(const char *name)
{

    for (int  i = 0; drop_these[i]; ++i)
    {
        if (strcmp(name, drop_these[i]) == 0)
        {
            return FALSE;
        }
    }
    // Bug 1516 - do not emit .debug_* sections if not -g
    if (Debug_Level < 1 && strncmp(name, ".debug_", 7) == 0)
        return FALSE;
    return TRUE;
}

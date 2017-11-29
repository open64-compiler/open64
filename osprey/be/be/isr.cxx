#include "elf_stuff.h"          /* for all Elf stuff */
#include <sys/elf_whirl.h>      /* for WHIRL sections */
#include "defs.h"           /* for wn_core.h */
#include "errors.h"
#include "opcode.h"         /* for wn_core.h */
#include "mempool.h"        /* for MEM_POOL (for maps) */
#include "strtab.h"         /* for strtab */
#include "symtab.h"         /* for symtab */
#include "const.h"          /* for constab */
#include "targ_const.h"     /* for tcon */
#include "config_targ.h"    /* for Target_ABI */
#include "config_debug.h"   /* for ir_version_check */
#include "config_elf_targ.h"
#include "irbdata.h"        /* for init_data */
#include "wn_core.h"        /* for WN */
#include "wn.h"             /* for max_region_id */
#include "wn_map.h"         /* for WN maps */
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"  /* for dst */
#include "pu_info.h"
#include "ir_elf.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"
#include "ir_bread.h"

#include "isr.h"

#define ERROR_RETURN -1

class ISR_NODE* isr_cg;

// Read the ISR call graph. pu_nums is the total PU number in the
// *.I file. Returns -1 if fails, else the size of the section.
// The function is only call when -ipisr is turned on.
INT
Read_isr_cg (ISR_NODE* cg, INT pu_num)
{
    register INT size;
    char *base;
    mINT32 *ptr;
    void *handle = local_fhandle;

    Set_Error_Phase ( "Reading WHIRL file" );

    OFFSET_AND_SIZE shdr = get_section (handle, SHT_MIPS_WHIRL, WT_CALLGRAPH);
    if (shdr.offset == 0) return ERROR_RETURN;

    base = (char*)handle + shdr.offset;
    size = shdr.size;
    ptr = (mINT32*)base; 
  
    for (INT i = 0; i < pu_num; i++) {
        mINT32 idx;
        cg[i].Set_Idx(i);
        while ((idx = *ptr++) != -1) // -1 marks the end of the caller list of a PU
            cg[i].Add_Parent(idx);
    }
    
    return size;
}

void Merge_Parents_Regset(ISR_NODE& node)
{
    ISA_REGISTER_CLASS rc;

    FOR_ALL_ISA_REGISTER_CLASS(rc)
    {
        REGISTER_SET set = REGISTER_SET_EMPTY_SET;
        BOOL first = 1;

        for (ISR_PARENT_ITER iter(node); !iter.End(); iter.Next())
        {
            mINT32 id = iter.Idx();
            ISR_NODE par = isr_cg[id];

            if (!par.Processed()) continue;

            if (first) {
                set = par.Regset(rc);
                first = 0;
            } else {
                set = REGISTER_SET_Intersection(set, par.Regset(rc)); 
            }
        } 

        node.Set_Regset(rc, set);
    }
}

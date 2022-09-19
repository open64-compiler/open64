/*
 * Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdint.h>
#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <unistd.h>		    /* for unlink() */
#ifdef __MINGW32__
#include <WINDOWS.h>
#else
#include <sys/mman.h>		    /* for mmap() */
#endif /* __MINGW32__ */
#include <errno.h>		    /* for errno */
#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>		    /* for all Elf stuff */
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>	    /* for WHIRL sections' sh_info */

#ifndef USE_STANDARD_TYPES
#define USE_STANDARD_TYPES	    /* override unwanted defines in "defs.h" */
#endif

#include "defs.h"
#include "erglob.h"
#include "errors.h"		    /* for ErrMsg() */
#include "opcode.h"
#include "mempool.h"
#include "wn.h"
#include "wn_map.h"
#include "strtab.h"		    /* for strtab */
#include "symtab.h"		    /* for symtab */
#include "irbdata.h"		    /* for inito */
#define USE_DST_INTERNALS
#include "dwarf_DST_mem.h"	    /* for dst */
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"

/* For 4K page, each kernel page maps to 4Mbytes user address space */
#define MAPPED_SIZE		0x400000
#define INIT_TMP_MAPPED_SIZE    MAPPED_SIZE

#ifdef BACK_END

#define DEFAULT_NUM_OF_PREFETCHES 64
WN **prefetch_ldsts;
INT num_prefetch_ldsts;
INT max_num_prefetch_ldsts;

#define DEFAULT_NUM_ALIAS_CLASSES 128
WN **alias_classes;
INT num_alias_class_nodes;
INT max_alias_class_nodes;

#define DEFAULT_NUM_ALIAS_CGNODES 128
WN **alias_cgnodes;
INT num_alias_cgnode_nodes;
INT max_alias_cgnode_nodes;

#define DEFAULT_NUM_AC_INTERNALS 128
WN **ac_internals;
INT num_ac_internal_nodes;
INT max_ac_internal_nodes;

#endif // BACK_END

#ifndef __GNUC__
#define __ALIGNOF(x) __builtin_alignof(x)
#else
#define __ALIGNOF(x) __alignof__(x)
#endif // __GNUC__

const char *Whirl_Revision = WHIRL_REVISION;

/* This variable is used by IPAA to pass its local map information
 * to ir_bwrite.c, and by ir_bread.c to pass it to WOPT:
 */
void *IPAA_Local_Map = NULL;

#if defined(KEY) && defined(BACK_END)
#include "be_ipa_util.h"
MOD_REF_INFO_TAB Mod_Ref_Info_Table;
#endif

BOOL Doing_mmapped_io = FALSE;

/* copy a block of memory into the temporary file.
 * "align" is the alignment requirement, which must be a power of 2.
 * "padding" is the byte offset from the beginning of the buffer that the
 * alignment requirement applies.
 *
 * It returns the file offset corresponding to buf + padding.
 */
extern off_t
ir_b_save_buf (const void *buf, Elf64_Word size, UINT32 align,
	       UINT32 padding, Output_File *fl)
{
    Current_Output = fl;

    off_t file_size = ir_b_align (fl->file_size, align, padding);

    if (file_size + size >= fl->mapped_size)
	ir_b_grow_map (file_size + size - fl->file_size, fl);
    
    Doing_mmapped_io = TRUE;
    memcpy (fl->map_addr + file_size, buf, size);
    Doing_mmapped_io = FALSE;
    fl->file_size = file_size + size;
    return file_size + padding;
} /* ir_b_save_buf */

/* copy a file into the temporary file.
 */
extern off_t
ir_b_copy_file (const void *buf, Elf64_Word size, void *tmpfl)
{
    Output_File* fl = (Output_File*)tmpfl;

    Current_Output = fl;

    if (size >= fl->mapped_size)
	ir_b_grow_map (size, fl);
    
    Doing_mmapped_io = TRUE;
    memcpy (fl->map_addr, buf, size);
    Doing_mmapped_io = FALSE;
    fl->file_size = size;
    return size;
} // ir_b_copy_file 


static void
save_buf_at_offset (const void *buf, Elf64_Word size, off_t offset,
		    Output_File *fl)
{
    Is_True (offset + size <= fl->mapped_size, ("Invalid buffer size"));

    Doing_mmapped_io = TRUE;
    memcpy (fl->map_addr + offset, buf, size);
    Doing_mmapped_io = FALSE;
} // save_buf_at_offset

// similar to ir_b_save_buf, except that no actual copying is done, but
// file space is reserved.
static off_t
ir_b_reserve_space (Elf64_Word size, unsigned int align, Output_File *fl)
{
    off_t file_size = ir_b_align (fl->file_size, align, 0);

    if (file_size + size >= fl->mapped_size)
	ir_b_grow_map (file_size + size - fl->file_size, fl);

    fl->file_size = file_size + size;
    return file_size;
} // ir_b_reserve_space

#ifndef __MINGW32__
/* increase the mmap size.  It is faster if we first unmap the region and
 * then map it again with a larger size.  The overhead for maintaining
 * multiple regions (by the kernel) outweighs that of extra system calls.
 * Also, we get a contiguous address space for the file this way.
 */
char *
ir_b_grow_map (Elf64_Word min_size, Output_File *fl)
{
    Is_True (fl->map_addr != 0, ("output file not yet mapped"));

    if (munmap (fl->map_addr, fl->mapped_size) == -1)
	ErrMsg (EC_IR_Write, fl->file_name, errno);
    min_size += fl->file_size;
    while (fl->mapped_size < min_size) {
	if (fl->mapped_size < MAPPED_SIZE)
	    fl->mapped_size = MAPPED_SIZE;
	else
	    fl->mapped_size += MAPPED_SIZE;
    }
#if !(defined(linux) || defined(BUILD_OS_DARWIN) || defined(__APPLE__))
    fl->map_addr = (char *) mmap (0, fl->mapped_size, PROT_READ|PROT_WRITE,
				  MAP_SHARED|MAP_AUTOGROW, fl->output_fd, 0); 
#else
    fl->map_addr = (char *) mmap (0, fl->mapped_size, PROT_READ|PROT_WRITE,
				  MAP_SHARED, fl->output_fd, 0); 

    if (ftruncate(fl->output_fd, fl->mapped_size))
	ErrMsg (EC_IR_Write, fl->file_name, strerror(errno));
#endif

    if (fl->map_addr == (char *) (-1))
	ErrMsg (EC_IR_Write, fl->file_name, strerror(errno));

    return fl->map_addr;
} /* ir_b_grow_map */

#else

#include<windows.h>
#include<io.h>
/* Increase the mmap size.  It is faster if we first unmap the region and
 * then map it again with a larger size.  The overhead for maintaining
 * multiple regions (by the kernel) outweighs that of extra system calls.
 * Also, we get a contiguous address space for the file this way.
 */
char *
ir_b_grow_map (Elf64_Word min_size, Output_File *fl)
{
	char errBuf[100];
    Is_True (fl->map_addr != 0, ("output file not yet mapped"));
	if(!UnmapViewOfFile(fl->map_addr) || !CloseHandle(fl->mapHd))
    	ErrMsg (EC_IR_Write, fl->file_name, strerror(errno));
    min_size += fl->file_size;
    while (fl->mapped_size < min_size) {
	if (fl->mapped_size < MAPPED_SIZE)
	    fl->mapped_size = MAPPED_SIZE;
	else
	    fl->mapped_size += MAPPED_SIZE;
    }

   if(_chsize(fl->output_fd, fl->mapped_size)){
    	FormatMessage(0,NULL, GetLastError(),0, errBuf, 100, NULL);
		ErrMsg (EC_IR_Write, fl->file_name, errBuf);
   }
    if( (fl->mapHd = CreateFileMapping((HANDLE)_get_osfhandle(fl->output_fd), 
	NULL, PAGE_READWRITE, 0, fl->mapped_size, fl->file_name))== NULL){
		FormatMessage(0,NULL, GetLastError(),0, errBuf, 100, NULL);
		ErrMsg (EC_IR_Write, fl->file_name, errBuf);
    	}

	if ( (fl->map_addr = (char *)MapViewOfFileEx(fl->mapHd, 
                          FILE_MAP_WRITE,0,0, fl->mapped_size, 0)) == NULL){
		FormatMessage(0,NULL, GetLastError(),0, errBuf, 100, NULL);
		ErrMsg (EC_IR_Write, fl->file_name, errBuf);
    	}
    if (fl->map_addr == (char *) (-1))
		ErrMsg (EC_IR_Write, fl->file_name, errBuf);
    return fl->map_addr;
} /* ir_b_grow_map */


#endif /* __MINGW32__ */


extern char *
ir_b_create_map (Output_File *fl)
{
    int fd = fl->output_fd;
    fl->mapped_size = INIT_TMP_MAPPED_SIZE;
#ifdef __MINGW32__
    {
      char errBuf[100];

      if( (fl->mapHd = CreateFileMapping((HANDLE)_get_osfhandle(fl->output_fd), NULL, PAGE_READWRITE, 0,
    								  fl->mapped_size, NULL))
    			   == NULL){
	  INT err = GetLastError();
	  FormatMessage(0,NULL, err,0, errBuf, 100, NULL);
          ErrMsg (EC_IR_Write, fl->file_name, errBuf);
    	  }
      if( (fl->map_addr = (char *)MapViewOfFileEx(fl->mapHd, FILE_MAP_WRITE,
    		  0,0,fl->mapped_size, 0))  == NULL){
		  FormatMessage(0,NULL, GetLastError(),0, errBuf, 100, NULL);
		  ErrMsg (EC_IR_Write, fl->file_name, errBuf);
    	  }
    }
#else
#if !defined(linux) && !defined(__APPLE__)
    fl->map_addr = (char *) mmap (0, fl->mapped_size, PROT_READ|PROT_WRITE,
				  MAP_SHARED|MAP_AUTOGROW, fd, 0); 
#else
    fl->map_addr = (char *) mmap (0, fl->mapped_size, PROT_READ|PROT_WRITE,
				  MAP_SHARED, fd, 0); 
#endif
#endif /* __MINGW32__ */
    return fl->map_addr;
} /* ir_b_create_map */


/* Walk the tree and copy it to contiguous memory block in the temp. file */
#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)

// ******************** IPA weak symbols^
#define IPA_get_symbol_file_array (*IPA_get_symbol_file_array_p)
#define Get_Node_From_PU (*Get_Node_From_PU_p)
#include <ipc_file.h>

extern IP_FILE_HDR_TABLE IP_File_header;
IP_FILE_HDR_TABLE *IP_File_header_p;
#define IP_File_header (*IP_File_header_p)

#include <ipo_tlog_utils.h>
#include <ipa_cg.h>
// ******************** IPA weak symbols$
extern off_t
ir_b_write_tree (WN *node, off_t base_offset, Output_File *fl, WN_MAP off_map, PU_Info *pu)
#else
extern off_t
ir_b_write_tree (WN *node, off_t base_offset, Output_File *fl, WN_MAP off_map)
#endif
{
    register OPCODE opcode;
    off_t node_offset;
    char *real_addr;
    INT32 size = WN_Size_and_StartAddress (node, (void **) &real_addr);

#define WN_ADDR(offset) ((WN *)(fl->map_addr + offset))

    node_offset = ir_b_save_buf (real_addr, size, __ALIGNOF(WN),
				 (char *)(node) - real_addr, fl); 

    opcode = (OPCODE) WN_opcode (node);

#ifdef BACK_END
    if (off_map != WN_MAP_UNDEFINED &&
	(Write_BE_Maps ||
	 Write_ALIAS_CLASS_Map ||
	 Write_ALIAS_CGNODE_Map ||
	 Write_AC_INTERNAL_Map)) {
	/* save node_offset for use when writing maps */
	BOOL set_offset = FALSE;
	OPERATOR opr = OPCODE_operator(opcode);

	if (Write_BE_Maps) {
	    if (opr == OPR_PREFETCH || opr == OPR_PREFETCHX ||
		OPCODE_is_load (opcode) || OPCODE_is_store (opcode))
		set_offset = TRUE;

	    /* check if the WN has a prefetch pointer */
	    if (WN_MAP_Get(WN_MAP_PREFETCH, node)) {
		/* make sure the prefetch_ldsts array is big enough to hold the
		   lds and sts plus one extra slot to mark the end */
		if (num_prefetch_ldsts == 0) {
		    max_num_prefetch_ldsts = DEFAULT_NUM_OF_PREFETCHES;
		    prefetch_ldsts =
			(WN **)MEM_POOL_Alloc (Malloc_Mem_Pool, max_num_prefetch_ldsts * sizeof(WN*));
		    FmtAssert (prefetch_ldsts,
			       ("No more memory for allocation."));
		} else if (max_num_prefetch_ldsts == num_prefetch_ldsts + 1) {
		    max_num_prefetch_ldsts *= 2;
		    prefetch_ldsts =
			(WN **)realloc(prefetch_ldsts,
				       max_num_prefetch_ldsts * sizeof(WN*));
		    FmtAssert (prefetch_ldsts,
			       ("No more memory for allocation."));
		}
		prefetch_ldsts[num_prefetch_ldsts] = node;
		num_prefetch_ldsts += 1;
	    }
	}
	    
	if (Write_ALIAS_CLASS_Map) {
	    if (OPCODE_is_store (opcode) ||
		OPCODE_is_load (opcode) ||
		(opr == OPR_LDA /* && LDA_has_ac_map_set(node) */) ||
		opr == OPR_PARM) {
	      set_offset = TRUE;
	    }

	    if (WN_MAP32_Get (WN_MAP_ALIAS_CLASS, node) != 0) {
		if (alias_classes == NULL) {
		    max_alias_class_nodes = DEFAULT_NUM_ALIAS_CLASSES;
		    alias_classes = (WN **) malloc (max_alias_class_nodes *
						    sizeof(WN *));
		    FmtAssert (alias_classes != NULL, ("No more memory."));
		} else if (max_alias_class_nodes == num_alias_class_nodes + 1) {
		    max_alias_class_nodes *= 2;
		    alias_classes = (WN **) realloc(alias_classes,
						    max_alias_class_nodes *
						    sizeof(WN **));
		    FmtAssert(alias_classes != NULL, ("No more memory."));
		}
		alias_classes[num_alias_class_nodes++] = node;
	    }
	}

        // To dump the WN to CGNodeId map for the Nystrom Alias Analyzer
	if (Write_ALIAS_CGNODE_Map) {

	    if (WN_MAP32_Get (WN_MAP_ALIAS_CGNODE, node) != 0) {
                set_offset = TRUE;
		if (alias_cgnodes == NULL) {
		    max_alias_cgnode_nodes = DEFAULT_NUM_ALIAS_CGNODES;
		    alias_cgnodes = (WN **) malloc (max_alias_cgnode_nodes *
						    sizeof(WN *));
		    FmtAssert (alias_cgnodes != NULL, ("No more memory."));
		} else if (max_alias_cgnode_nodes == 
                           num_alias_cgnode_nodes + 1) {
		    max_alias_cgnode_nodes *= 2;
		    alias_cgnodes = (WN **) realloc(alias_cgnodes,
						    max_alias_cgnode_nodes *
						    sizeof(WN **));
		    FmtAssert(alias_cgnodes != NULL, ("No more memory."));
		}
		alias_cgnodes[num_alias_cgnode_nodes++] = node;
	    }
	}
	
	if (Write_AC_INTERNAL_Map) {
	    if (opr == OPR_ILOAD  ||
		opr == OPR_MLOAD  ||
		opr == OPR_PARM   ||
		opr == OPR_ISTORE ||
		opr == OPR_MSTORE) {
	      set_offset = TRUE;
	    }

	    if (WN_MAP_Get (WN_MAP_AC_INTERNAL, node) != NULL) {
		if (ac_internals == NULL) {
		    max_ac_internal_nodes = DEFAULT_NUM_AC_INTERNALS;
		    ac_internals = (WN **) malloc (max_ac_internal_nodes *
						   sizeof(WN *));
		    FmtAssert (ac_internals != NULL, ("No more memory."));
		} else if (max_ac_internal_nodes == num_ac_internal_nodes + 1) {
		    max_ac_internal_nodes *= 2;
		    ac_internals = (WN **) realloc(ac_internals,
						   max_ac_internal_nodes *
						   sizeof(WN **));
		    FmtAssert(ac_internals != NULL, ("No more memory."));
		}
		ac_internals[num_ac_internal_nodes++] = node;
	    }
	}

	if (set_offset)
	    WN_MAP32_Set(off_map, node, node_offset - base_offset);

    }
#endif /* BACK_END */
#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)
// ONLY for IPA.
    if (Get_ipa_tlog_phase() == PHASE_IPA && WN_operator(node) == OPR_REGION
        && WN_region_is_EH (node) && WN_block_empty (WN_region_pragmas (node)))
    {
      FmtAssert (pu, ("Null pu info"));

      IPA_NODE * cg_node = Get_Node_From_PU (pu);
      Is_True (cg_node, ("Null ipa node"));

      if (!cg_node->Is_PU_Write_Complete() && !cg_node->EHinfo_Updated())
      {
	// Note: Don't set EHinfo_Updated here, as we are not yet done with
	// the entire PU.
	Is_True (PU_src_lang (cg_node->Get_PU()) & PU_CXX_LANG, 
		   ("Exception region in non-C++ PU"));

	int sym_size;
	SUMMARY_SYMBOL * sym_array = IPA_get_symbol_file_array (cg_node->File_Header(), sym_size);
	Is_True (sym_array != NULL, ("Missing SUMMARY_SYMBOL section"));
    	INITV_IDX types = INITV_next (INITV_blk (INITO_val (WN_ereg_supp (node))));
	for (; types; types = INITV_next (types))
	{
	    if (INITV_kind (types) != INITVKIND_VAL)
	    	continue;
	    int index = TCON_uval (INITV_tc_val (types));
	    if (index <= 0) continue;
	    ST_IDX new_idx = sym_array[index].St_idx();
	    INITV_IDX next = INITV_next (types);	// for backup
	    INITV_Set_VAL (Initv_Table[types], Enter_tcon (
	    		   Host_To_Targ (MTYPE_U4, new_idx)), 1);
	    Set_INITV_next (types, next);
    	}
      }
    }
#endif


    if (opcode == OPC_BLOCK) {
	register off_t prev, this_node;

	if (WN_first(node) == 0) {
	    WN_first(WN_ADDR(node_offset)) = (WN *) -1;
	    WN_last(WN_ADDR(node_offset)) = (WN *) -1;
	} else {
	    register WN *wn = WN_first (node);
#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)
	    prev = ir_b_write_tree(wn, base_offset, fl, off_map, pu);
#else
	    prev = ir_b_write_tree(wn, base_offset, fl, off_map);
#endif
	    WN_first(WN_ADDR(node_offset)) = (WN *) (INTPTR) prev;

	    while (wn = WN_next(wn)) {
#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)
		this_node = ir_b_write_tree(wn, base_offset, fl, off_map, pu);
#else
		this_node = ir_b_write_tree(wn, base_offset, fl, off_map);
#endif
		/* fill in the correct next/prev offsets (in place of -1) */
		WN_next(WN_ADDR(prev + base_offset)) = (WN *) (INTPTR) this_node;
		WN_prev(WN_ADDR(this_node + base_offset)) = (WN *)(INTPTR)prev;
		prev = this_node;
	    }

	    WN_last(WN_ADDR(node_offset)) = (WN *) (INTPTR) prev;
	}
    } else if (!OPCODE_is_leaf(opcode)) {
	register int i;

	for (i = 0; i < WN_kid_count(node); i++) {
	    register off_t kid;

	    if (WN_kid(node, i) == 0) {
		WN_kid(WN_ADDR(node_offset), i) = (WN *) -1;
	    } else {
#if defined(KEY) && !defined(FRONT_END) && !defined(IR_TOOLS)
		kid = ir_b_write_tree (WN_kid(node, i), base_offset,
				       fl, off_map, pu);
#else
		kid = ir_b_write_tree (WN_kid(node, i), base_offset,
				       fl, off_map);
#endif
		WN_kid(WN_ADDR(node_offset), i) = (WN *) (INTPTR) kid;
	    }
	}
    }

    if (OPCODE_has_next_prev(opcode)) {
	/* just set the default values for now */
	WN_prev(WN_ADDR(node_offset)) = (WN *) -1;
	WN_next(WN_ADDR(node_offset)) = (WN *) -1;
    }

    return node_offset - base_offset;
} /* ir_b_write_tree */


/*------------ symtab routines ---------------*/

// function object for writing out various symbol tables
template <class T>
struct WRITE_TABLE_OP
{
    Output_File *fl;

    void operator () (UINT, T *t, UINT size) const {
	(void) ir_b_save_buf (t, size * sizeof(T), __ALIGNOF(T), 0, fl); 
    }

    WRITE_TABLE_OP (Output_File *_fl) : fl (_fl) {}
}; // WRITE_TABLE_OP


template <class TABLE>
static off_t
write_table (TABLE& fld, off_t base_offset,
	     Output_File *fl)
{
    off_t cur_offset = ir_b_align (fl->file_size, __ALIGNOF(typename TABLE::base_type),
				   0);
    fl->file_size = ir_b_align (fl->file_size, __ALIGNOF(typename TABLE::base_type), 0);

#ifndef __GNUC__
    const WRITE_TABLE_OP<TABLE::base_type> write_table_op(fl);
#else
    const WRITE_TABLE_OP<typename TABLE::base_type> write_table_op(fl);
#endif

    For_all_blocks (fld, write_table_op);

    return cur_offset - base_offset;
} // write_table


static off_t
write_file_info (off_t base_offset, Output_File *fl)
{
    off_t cur_offset = ir_b_align (fl->file_size, __ALIGNOF(FILE_INFO), 0);
    ir_b_save_buf (&File_info, sizeof(File_info), __ALIGNOF(File_info), 0, fl);

    return cur_offset - base_offset;
} // write_file_info


// write a global symtab:  
off_t
ir_b_write_global_symtab (off_t base_offset, Output_File *fl)
{
    GLOBAL_SYMTAB_HEADER_TABLE gsymtab;

    // should use __builtin_alignof(gsymtab) instead of sizeof(mUINT64),
    // but our frontend has a bug and fails to compile it. 
    const  off_t symtab_offset =
	ir_b_reserve_space (sizeof(gsymtab), sizeof(mUINT64), fl);

    off_t cur_offset;
    UINT i = 0;
    const UINT idx = GLOBAL_SYMTAB;

    cur_offset = write_file_info (symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, sizeof(FILE_INFO),
			      sizeof(FILE_INFO), __ALIGNOF(FILE_INFO),
			      SHDR_FILE);

    cur_offset = write_table (*(Scope_tab[idx].st_tab), symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset,
			      Scope_tab[idx].st_tab->Size () * sizeof(ST),
			      sizeof(ST), __ALIGNOF(ST), SHDR_ST);

    // call fix_array_ty?

    cur_offset = write_table (Ty_tab, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Ty_tab.Size () * sizeof(TY),
			      sizeof(TY), __ALIGNOF(TY), SHDR_TY);

    cur_offset = write_table (Pu_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Pu_Table.Size () * sizeof(PU),
			      sizeof(PU), __ALIGNOF(PU), SHDR_PU);

    cur_offset = write_table (Fld_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Fld_Table.Size () * sizeof(FLD),
			      sizeof(FLD), __ALIGNOF(FLD), SHDR_FLD);

    cur_offset = write_table (Arb_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Arb_Table.Size () * sizeof(ARB),
			      sizeof(ARB), __ALIGNOF(ARB), SHDR_ARB);

    cur_offset = write_table (Tylist_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset,
			      Tylist_Table.Size () * sizeof(TYLIST),
			      sizeof(TYLIST), __ALIGNOF(TYLIST), SHDR_TYLIST); 

    cur_offset = write_table (Tcon_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Tcon_Table.Size () * sizeof(TCON),
			      sizeof(TCON), __ALIGNOF(TCON), SHDR_TCON); 

    cur_offset = ir_b_save_buf (TCON_strtab_buffer (), TCON_strtab_size (),
				1, 0, fl) - symtab_offset;
    gsymtab.header[i++].Init (cur_offset, TCON_strtab_size (), 1, 1, SHDR_STR);

    cur_offset = write_table (*(Scope_tab[idx].inito_tab), symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset,
			      Scope_tab[idx].inito_tab->Size () * sizeof(INITO),
			      sizeof(INITO), __ALIGNOF(INITO), SHDR_INITO);

    cur_offset = write_table (Initv_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Initv_Table.Size () * sizeof(INITV),
			      sizeof(INITV), __ALIGNOF(INITV), SHDR_INITV); 

    cur_offset = write_table (Blk_Table, symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset, Blk_Table.Size () * sizeof(BLK),
			      sizeof(BLK), __ALIGNOF(BLK), SHDR_BLK); 

    cur_offset = write_table (*(Scope_tab[idx].st_attr_tab), symtab_offset, fl);
    gsymtab.header[i++].Init (cur_offset,
			      Scope_tab[idx].st_attr_tab->Size () * sizeof(ST_ATTR),
			      sizeof(ST_ATTR), __ALIGNOF(ST_ATTR), SHDR_ST_ATTR);

    save_buf_at_offset (&gsymtab, sizeof(gsymtab), symtab_offset, fl);

    return symtab_offset - base_offset;

} // ir_b_write_global_symtab

off_t
ir_b_write_local_symtab (const SCOPE& pu, off_t base_offset, Output_File *fl)
{
    LOCAL_SYMTAB_HEADER_TABLE symtab;

    const off_t symtab_offset =
	ir_b_reserve_space (sizeof(symtab), sizeof(mUINT64), fl);

    UINT i = 0;
    off_t cur_offset;

    cur_offset = write_table (*pu.st_tab, symtab_offset, fl);
    symtab.header[i++].Init (cur_offset, pu.st_tab->Size () * sizeof(ST),
			     sizeof(ST), __ALIGNOF(ST), SHDR_ST);

    cur_offset = write_table (*pu.label_tab, symtab_offset, fl);
    symtab.header[i++].Init (cur_offset, pu.label_tab->Size () * sizeof(LABEL),
			     sizeof(LABEL), __ALIGNOF(LABEL), SHDR_LABEL); 

    cur_offset = write_table (*pu.preg_tab, symtab_offset, fl);
    symtab.header[i++].Init (cur_offset, pu.preg_tab->Size () * sizeof(PREG),
			     sizeof(PREG), __ALIGNOF(PREG), SHDR_PREG);

    cur_offset = write_table (*pu.inito_tab, symtab_offset, fl);
    symtab.header[i++].Init (cur_offset, pu.inito_tab->Size () * sizeof(INITO),
			     sizeof(INITO), __ALIGNOF(INITO), SHDR_INITO);
    
    cur_offset = write_table (*pu.st_attr_tab, symtab_offset, fl);
    symtab.header[i++].Init (cur_offset, pu.st_attr_tab->Size () * sizeof(ST_ATTR),
			     sizeof(ST_ATTR), __ALIGNOF(ST_ATTR), SHDR_ST_ATTR);
    
    save_buf_at_offset (&symtab, sizeof(symtab), symtab_offset, fl);

    return symtab_offset - base_offset;

} // ir_b_write_local_symtab


/* write blocks of data, then block headers, then # blocks */
extern off_t
ir_b_write_dst (DST_TYPE dst, off_t base_offset, Output_File *fl)
{
    off_t cur_offset;
    DST_BLOCK_IDX i;
    block_header *dst_blocks;
    Current_DST = dst;

    dst_blocks = ((DST_Type *)dst)->dst_blocks;
    FOREACH_DST_BLOCK(i) {
	/* may have 64-bit data fields, so align at 8 bytes */
	cur_offset = ir_b_save_buf (dst_blocks[i].offset, 
		dst_blocks[i].size, __ALIGNOF(INT64), 0, fl);
	// off_t may be larger than pointer (cygwin), so first convert to INTPS
	dst_blocks[i].offset = (char*)(INTPS)(cur_offset - base_offset);
    } 
    FOREACH_DST_BLOCK(i) {
	cur_offset = ir_b_save_buf
	    ((char*)&dst_blocks[i], sizeof(block_header),
	     __ALIGNOF(block_header), 0, fl);
    } 
    cur_offset = ir_b_save_buf
	((char*)&((DST_Type *)dst)->last_block_header, sizeof(mINT32), 
	 __ALIGNOF(INT32), 0, fl);
    return cur_offset - base_offset;
}

#if defined(KEY) && defined(BACK_END)

#define HEADER_ADDR(offset) \
 ((Elf64_Word*)(fl->map_addr + offset))

// Write IPA mod/ref information into file fl. This function should
// only be called if there is any mod/ref info to write.
void
IPA_irb_write_mod_ref_info(Output_File *fl)
{
    INT offset, header_loc;

    INT offset_mod_ref = 0;

    Elf64_Word temp;

    INT size = Mod_Ref_Info_Table_Size();

    FmtAssert (size, ("IPA_irb_write_mod_ref_info: No MOD/REF information"));

    INT cur_sec_disp = fl->file_size;

    // store the offset of the header structure in this field
    header_loc = (INT) ir_b_save_buf(&temp, sizeof(Elf64_Word),
                                     sizeof(INT64),0,fl);

    // 0th entry, store the offset
    // ** NOTE **: Only the first copy should be aligned.
    // pu_idx
    offset_mod_ref = ir_b_save_buf (
                       &Mod_Ref_Info_Table[0].pu_idx,
                       sizeof(PU_IDX),
                       sizeof(INT64),
                       0,
                       fl);

    // mod/ref size
    ir_b_save_buf (&Mod_Ref_Info_Table[0].size,
                   sizeof(mUINT32),
                   0,
                   0,
                   fl);

    // mod
    ir_b_save_buf (Mod_Ref_Info_Table[0].mod,
                   Mod_Ref_Info_Table[0].size,
                   0, // alignment
                   0, // padding
                   fl);

    // ref
    ir_b_save_buf (Mod_Ref_Info_Table[0].ref,
                   Mod_Ref_Info_Table[0].size,
                   0, // alignment
                   0, // padding
                   fl);

    // same_entry_exit_value_or_1
    ir_b_save_buf (Mod_Ref_Info_Table[0].same_entry_exit_value_or_1,
		   Mod_Ref_Info_Table[0].size,
		   0, // alignment
		   0, // padding
		   fl);

    for (INT i=1; i<size; i++)
    {
      ir_b_save_buf (&Mod_Ref_Info_Table[i].pu_idx,
                     sizeof(PU_IDX), 0, 0, fl);

      ir_b_save_buf (&Mod_Ref_Info_Table[i].size,
                     sizeof(mUINT32), 0, 0, fl);

      ir_b_save_buf (Mod_Ref_Info_Table[i].mod,
                     Mod_Ref_Info_Table[i].size, 0, 0, fl);

      ir_b_save_buf (Mod_Ref_Info_Table[i].ref,
                     Mod_Ref_Info_Table[i].size, 0, 0, fl);

      ir_b_save_buf (Mod_Ref_Info_Table[i].same_entry_exit_value_or_1,
                     Mod_Ref_Info_Table[i].size, 0, 0, fl);
      
    }

    offset_mod_ref = offset_mod_ref - cur_sec_disp;

    BE_SUMMARY_HEADER header;
    offset = (INT)ir_b_save_buf(&header, sizeof(BE_SUMMARY_HEADER),
                                sizeof(INT64), 0, fl);

    *(HEADER_ADDR(header_loc)) = offset - cur_sec_disp;
    BE_SUMMARY_HEADER *header_addr =
                      (BE_SUMMARY_HEADER *)(fl->map_addr + offset);

    header_addr->offset = offset_mod_ref;
    header_addr->size = size;
    header_addr->entsize = sizeof(pu_mod_ref_info);
}
#endif


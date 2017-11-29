/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000,2004 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1500 Crittenden Lane,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#include "config.h"
#include "dwarf_stuff.h"
#include "libdwarfdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef   HAVE_ELFACCESS_H
#include <elfaccess.h>
#endif
#include "pro_incl.h"
#include "pro_section.h"
#include "pro_line.h"
#include "pro_frame.h"
#include "pro_die.h"
#include "pro_macinfo.h"
#include "pro_types.h"

#ifndef SHF_MIPS_NOSTRIP
/* if this is not defined, we probably don't need it: just use 0 */
#define SHF_MIPS_NOSTRIP 0
#endif
#ifndef R_MIPS_NONE
#define R_MIPS_NONE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* must match up with pro_section.h defines of DEBUG_INFO etc 
and sectnames (below)
*/
char *_dwarf_rel_section_names[] = {
    ".rel.debug_info",
    ".rel.debug_line",
    ".rel.debug_abbrev",	/* no relocations on this, really */
    ".rel.debug_frame",
    ".rel.debug_aranges",
    ".rel.debug_pubnames",
    ".rel.debug_str",
    ".rel.debug_funcnames",	/* sgi extension */
    ".rel.debug_typenames",	/* sgi extension */
    ".rel.debug_varnames",	/* sgi extension */
    ".rel.debug_weaknames",	/* sgi extension */
    ".rel.debug_macinfo",
    ".rel.debug_loc",
    ".rel.eh_frame",
};

/* names of sections. Ensure that it matches the defines 
   in pro_section.h, in the same order 
   Must match also _dwarf_rel_section_names above
*/
char *_dwarf_sectnames[] = {
#ifdef KEY /* Mac port */
    DEBUG_INFO_SECTNAME,
    DEBUG_LINE_SECTNAME,
    DEBUG_ABBREV_SECTNAME,
    DEBUG_FRAME_SECTNAME,
    DEBUG_ARANGES_SECTNAME,
    DEBUG_PUBNAMES_SECTNAME,
    DEBUG_STR_SECTNAME,
    DEBUG_FUNCNAMES_SECTNAME,	/* sgi extension */
    DEBUG_TYPENAMES_SECTNAME,	/* sgi extension */
    DEBUG_VARNAMES_SECTNAME,	/* sgi extension */
    DEBUG_WEAKNAMES_SECTNAME,	/* sgi extension */
    DEBUG_MACINFO_SECTNAME,
    DEBUG_LOC_SECTNAME,
    EH_FRAME_SECTNAME
#else /* KEY Mac port */
    ".debug_info",
    ".debug_line",
    ".debug_abbrev",
    ".debug_frame",
    ".debug_aranges",
    ".debug_pubnames",
    ".debug_str",
    ".debug_funcnames",		/* sgi extension */
    ".debug_typenames",		/* sgi extension */
    ".debug_varnames",		/* sgi extension */
    ".debug_weaknames",		/* sgi extension */
    ".debug_macinfo",
    ".debug_loc",
    ".eh_frame"
#endif /* KEY Mac port */
};




static Dwarf_Ubyte std_opcode_len[] = { 0,	/* DW_LNS_copy */
    1,				/* DW_LNS_advance_pc */
    1,				/* DW_LNS_advance_line */
    1,				/* DW_LNS_set_file */
    1,				/* DW_LNS_set_column */
    0,				/* DW_LNS_negate_stmt */
    0,				/* DW_LNS_set_basic_block */
    0,				/* DW_LNS_const_add_pc */
    1,				/* DW_LNS_fixed_advance_pc */
};

/* struct to hold relocation entries. Its mantained as a linked
   list of relocation structs, and will then be written at as a
   whole into the relocation section. Whether its 32 bit or
   64 bit will be obtained from Dwarf_Debug pointer.
*/

typedef struct Dwarf_P_Rel_s *Dwarf_P_Rel;
struct Dwarf_P_Rel_s {
    Dwarf_P_Rel dr_next;
    void *dr_rel_datap;
};
typedef struct Dwarf_P_Rel_Head_s *Dwarf_P_Rel_Head;
struct Dwarf_P_Rel_Head_s {
    struct Dwarf_P_Rel_s *drh_head;
    struct Dwarf_P_Rel_s *drh_tail;
};

int _dwf_pro_generate_ehframe(Dwarf_P_Debug dbg, Dwarf_Error * error);
static int _dwarf_pro_generate_debugline(Dwarf_P_Debug dbg,
					 Dwarf_Error * error);
static int _dwarf_pro_generate_debugframe(Dwarf_P_Debug dbg,
					  Dwarf_Error * error);
static int _dwarf_pro_generate_debuginfo(Dwarf_P_Debug dbg,
					 Dwarf_Error * error);
static Dwarf_P_Abbrev _dwarf_pro_getabbrev(Dwarf_P_Die, Dwarf_P_Abbrev);
static int _dwarf_pro_match_attr
    (Dwarf_P_Attribute, Dwarf_P_Abbrev, int no_attr);

/* these macros used as return value for below functions */
#define		OPC_INCS_ZERO		-1
#define		OPC_OUT_OF_RANGE	-2
#define		LINE_OUT_OF_RANGE	-3
static int _dwarf_pro_get_opc(Dwarf_Unsigned addr_adv, int line_adv);



/*
	Return TRUE if we need the section, FALSE otherwise

        If any of the 'line-data-related' calls were made
        including file or directory entries,
        produce .debug_line .

*/
static int
dwarf_need_debug_line_section(Dwarf_P_Debug dbg)
{
    if (dbg->de_lines == NULL && dbg->de_file_entries == NULL
	&& dbg->de_inc_dirs == NULL) {
	return FALSE;
    }
    return TRUE;
}

/*
    Convert debug information to  a format such that 
    it can be written on disk.
    Called exactly once per execution.
*/
Dwarf_Signed
dwarf_transform_to_disk_form(Dwarf_P_Debug dbg, Dwarf_Error * error)
{
    /* 
       Section data in written out in a number of buffers. Each
       _generate_*() function returns a cumulative count of buffers for 
       all the sections. get_section_bytes() returns pointers to these 
       buffers one at a time. */
    int nbufs;
    int sect;
    int name_idx;
    int err;
    Dwarf_Unsigned du;

    if (dbg->de_version_magic_number != PRO_VERSION_MAGIC) {
	DWARF_P_DBG_ERROR(dbg, DW_DLE_IA, DW_DLV_NOCOUNT);
    }

    /* Create dwarf section headers */
    for (sect = 0; sect < NUM_DEBUG_SECTIONS; sect++) {
	long flags = 0;

	switch (sect) {

	case DEBUG_INFO:
	    if (dbg->de_dies == NULL)
		continue;
	    break;

	case DEBUG_LINE:
	    if (dwarf_need_debug_line_section(dbg) == FALSE) {
		continue;
	    }
	    break;

	case DEBUG_ABBREV:
	    if (dbg->de_dies == NULL)
		continue;
	    break;

	case DEBUG_FRAME:
	    if (dbg->de_frame_cies == NULL)
		continue;
	    flags = SHF_MIPS_NOSTRIP;
	    break;

	case DEBUG_ARANGES:
	    if (dbg->de_arange == NULL)
		continue;
	    break;

	case DEBUG_PUBNAMES:
	    if (dbg->de_simple_name_headers[dwarf_snk_pubname].
		sn_head == NULL)
		continue;
	    break;

	case DEBUG_STR:
	    if (dbg->de_strings == NULL)
		continue;
	    break;

	case DEBUG_FUNCNAMES:
	    if (dbg->de_simple_name_headers[dwarf_snk_funcname].
		sn_head == NULL)
		continue;
	    break;

	case DEBUG_TYPENAMES:
	    if (dbg->de_simple_name_headers[dwarf_snk_typename].
		sn_head == NULL)
		continue;
	    break;

	case DEBUG_VARNAMES:
	    if (dbg->de_simple_name_headers[dwarf_snk_varname].
		sn_head == NULL)
		continue;
	    break;

	case DEBUG_WEAKNAMES:
	    if (dbg->de_simple_name_headers[dwarf_snk_weakname].
		sn_head == NULL)
		continue;
	    break;

	case DEBUG_MACINFO:
	    if (dbg->de_first_macinfo == NULL)
		continue;
	    break;
	case DEBUG_LOC:
	    /* not handled yet */
	    continue;
	case EH_FRAME:
	    if (dbg->de_eh_frame_cies == NULL)
		continue;
#ifdef KEY
	    flags = SHF_MIPS_NOSTRIP | SHF_ALLOC;
#else
	    flags = SHF_MIPS_NOSTRIP;
#endif
	    break;
	default:
	    /* logic error: missing a case */
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR, DW_DLV_NOCOUNT);
	}
	{
	    int new_base_elf_sect;

	    if (dbg->de_func_b) {
		new_base_elf_sect =
		    dbg->de_func_b(_dwarf_sectnames[sect],
				   /* rec size */ 1,
				   SECTION_TYPE,
				   flags, SHN_UNDEF, 0, &du, &err);

	    } else {
		new_base_elf_sect = dbg->de_func(_dwarf_sectnames[sect],
						 dbg->
						 de_relocation_record_size,
						 SECTION_TYPE, flags,
						 SHN_UNDEF, 0,
						 &name_idx, &err);
		du = name_idx;
	    }
	    if (new_base_elf_sect == -1) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR,
				  DW_DLV_NOCOUNT);
	    }
	    dbg->de_elf_sects[sect] = new_base_elf_sect;

	    dbg->de_sect_name_idx[sect] = du;
	}
    }

    nbufs = 0;

    /* 
       Changing the order in which the sections are generated may
       cause problems because of relocations. */

    if (dwarf_need_debug_line_section(dbg) == TRUE) {
	nbufs = _dwarf_pro_generate_debugline(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGLINE_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_frame_cies) {
	nbufs = _dwarf_pro_generate_debugframe(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGFRAME_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }
    if (dbg->de_first_macinfo) {
	nbufs = _dwarf_pro_transform_macro_info_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGMACINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_dies) {
	nbufs = _dwarf_pro_generate_debuginfo(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_arange) {
	nbufs = _dwarf_transform_arange_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_simple_name_headers[dwarf_snk_pubname].sn_head) {
	nbufs = _dwarf_transform_simplename_to_disk(dbg,
						    dwarf_snk_pubname,
						    DEBUG_PUBNAMES,
						    error);


	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_simple_name_headers[dwarf_snk_funcname].sn_head) {
	nbufs = _dwarf_transform_simplename_to_disk(dbg,
						    dwarf_snk_funcname,
						    DEBUG_FUNCNAMES,
						    error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_simple_name_headers[dwarf_snk_typename].sn_head) {
	nbufs = _dwarf_transform_simplename_to_disk(dbg,
						    dwarf_snk_typename,
						    DEBUG_TYPENAMES,
						    error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_simple_name_headers[dwarf_snk_varname].sn_head) {
	nbufs = _dwarf_transform_simplename_to_disk(dbg,
						    dwarf_snk_varname,
						    DEBUG_VARNAMES,
						    error);

	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_simple_name_headers[dwarf_snk_weakname].sn_head) {
	nbufs = _dwarf_transform_simplename_to_disk(dbg,
						    dwarf_snk_weakname,
						    DEBUG_WEAKNAMES,
						    error);

	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_eh_frame_cies) {
	nbufs = _dwf_pro_generate_ehframe(dbg,error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGFRAME_ERROR, DW_DLV_NOCOUNT);
	}
    }

    {
	Dwarf_Signed new_secs;
	int res;

	res = dbg->de_transform_relocs_to_disk(dbg, &new_secs);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR,
			      DW_DLV_NOCOUNT);
	}
	nbufs += new_secs;
    }
    return nbufs;
}


/*---------------------------------------------------------------
	Generate debug_line section 
---------------------------------------------------------------*/
static int
_dwarf_pro_generate_debugline(Dwarf_P_Debug dbg, Dwarf_Error * error)
{
    Dwarf_P_Inc_Dir curdir;
    Dwarf_P_F_Entry curentry;
    Dwarf_P_Line curline, prevline;

    /* all data named cur* are used to loop thru linked lists */

    int sum_bytes;
    int prolog_size;
    unsigned char *data;	/* holds disk form data */
    int elfsectno;
    unsigned char *start_line_sec;	/* pointer to the buffer at
					   section start */
    /* temps for memcpy */
    Dwarf_Unsigned du;
    Dwarf_Ubyte db;
    Dwarf_Half dh;
    int res;
    int uwordb_size = dbg->de_offset_size;
    int extension_size = dbg->de_64bit_extension ? 4 : 0;
    int upointer_size = dbg->de_pointer_size;
    char buff1[ENCODE_SPACE_NEEDED];


    sum_bytes = 0;

    elfsectno = dbg->de_elf_sects[DEBUG_LINE];

    /* statement prologue information */
    prolog_size = 0;
    /* include directories */
    curdir = dbg->de_inc_dirs;
    while (curdir) {
	prolog_size += strlen(curdir->did_name) + 1;
	curdir = curdir->did_next;
    }
    prolog_size++;		/* last null following last directory
				   entry. */

    /* file entries */
    curentry = dbg->de_file_entries;
    while (curentry) {
	prolog_size +=
	    strlen(curentry->dfe_name) + 1 + curentry->dfe_nbytes;
	curentry = curentry->dfe_next;
    }
    prolog_size++;		/* last null byte */


    prolog_size += extension_size +	/* DISTINGUISHED VALUE */
	sizeof_uhalf(dbg) +	/* version # */
	uwordb_size +		/* prologue length */
	sizeof_ubyte(dbg) +	/* min_instr length */
	sizeof_ubyte(dbg) +	/* default is_stmt */
	sizeof_ubyte(dbg) +	/* linebase */
	sizeof_ubyte(dbg) +	/* linerange */
	sizeof_ubyte(dbg);	/* opcode base */

    /* length of table specifying # of opnds */
    prolog_size += sizeof(std_opcode_len);
    prolog_size += uwordb_size;	/* for total length field */

    GET_CHUNK(dbg, elfsectno, data, prolog_size, error);
    start_line_sec = data;

    /* copy over the data */
    /* total_length */
    du = 0;
    if (extension_size) {
	Dwarf_Word x = DISTINGUISHED_VALUE;

	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &x,
			sizeof(x), extension_size);
	data += extension_size;
    }

    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &du,
		    sizeof(du), uwordb_size);
    data += uwordb_size;

    dh = VERSION;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &dh,
		    sizeof(dh), sizeof(Dwarf_Half));
    data += sizeof(Dwarf_Half);

    /* prologue length */
    du = prolog_size - (uwordb_size + sizeof(Dwarf_Half) + uwordb_size);
    {
	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &du,
			sizeof(du), uwordb_size);
	data += uwordb_size;
    }
    db = MIN_INST_LENGTH;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), sizeof(Dwarf_Ubyte));
    data += sizeof(Dwarf_Ubyte);
    db = DEFAULT_IS_STMT;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), sizeof(Dwarf_Ubyte));
    data += sizeof(Dwarf_Ubyte);
    db = (Dwarf_Ubyte) LINE_BASE;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), sizeof(Dwarf_Ubyte));
    data += sizeof(Dwarf_Ubyte);
    db = LINE_RANGE;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), sizeof(Dwarf_Ubyte));
    data += sizeof(Dwarf_Ubyte);
    db = OPCODE_BASE;
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), sizeof(Dwarf_Ubyte));
    data += sizeof(Dwarf_Ubyte);
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) std_opcode_len,
		    sizeof(std_opcode_len), sizeof(std_opcode_len));
    data += sizeof(std_opcode_len);

    /* copy over include directories */
    curdir = dbg->de_inc_dirs;
    while (curdir) {
	strcpy((char *) data, curdir->did_name);
	data += strlen(curdir->did_name) + 1;
	curdir = curdir->did_next;
    }
    *data = '\0';		/* last null */
    data++;

    /* copy file entries */
    curentry = dbg->de_file_entries;
    while (curentry) {
	strcpy((char *) data, curentry->dfe_name);
	data += strlen(curentry->dfe_name) + 1;
	/* copies of leb numbers, no endian issues */
	memcpy((void *) data,
	       (const void *) curentry->dfe_args, curentry->dfe_nbytes);
	data += curentry->dfe_nbytes;
	curentry = curentry->dfe_next;
    }
    *data = '\0';
    data++;

    sum_bytes += prolog_size;

    curline = dbg->de_lines;
    prevline = (Dwarf_P_Line)
	_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Line_s));
    if (prevline == NULL) {
	DWARF_P_DBG_ERROR(dbg, DW_DLE_LINE_ALLOC, -1);
    }
    _dwarf_pro_reg_init(prevline);
    /* generate opcodes for line numbers */
    while (curline) {
	int nbytes;
	char *arg;
	int opc;
	int no_lns_copy;	/* if lns copy opcode doesnt need to be 
				   generated, if special opcode or end 
				   sequence */
	Dwarf_Unsigned addr_adv;
	int line_adv;		/* supposed to be a reasonably small
				   number, so the size should not be a
				   problem. ? */

	no_lns_copy = 0;
	if (curline->dpl_opc != 0) {
	    int inst_bytes;	/* no of bytes in extended opcode */
	    char *str;		/* hold leb encoded inst_bytes */
	    int str_nbytes;	/* no of bytes in str */

	    switch (curline->dpl_opc) {
	    case DW_LNE_end_sequence:

		/* Advance pc to end of text section. */
		addr_adv = curline->dpl_address - prevline->dpl_address;
		if (addr_adv > 0) {
		    db = DW_LNS_advance_pc;
		    res =
			_dwarf_pro_encode_leb128_nm(addr_adv /
						    MIN_INST_LENGTH,
						    &nbytes, buff1,
						    sizeof(buff1));
		    if (res != DW_DLV_OK) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		    }
		    GET_CHUNK(dbg, elfsectno, data,
			      nbytes + sizeof(Dwarf_Ubyte), error);
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &db, sizeof(db),
				    sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    /* leb, no endianness issue */
		    memcpy((void *) data, (const void *) buff1, nbytes);
		    data += nbytes + sizeof(Dwarf_Ubyte);
		    sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		    prevline->dpl_address = curline->dpl_address;
		}

		/* first null byte */
		db = 0;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);

		/* write length of extended opcode */
		inst_bytes = sizeof(Dwarf_Ubyte);
		res =
		    _dwarf_pro_encode_leb128_nm(inst_bytes, &str_nbytes,
						buff1, sizeof(buff1));
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		}
		GET_CHUNK(dbg, elfsectno, data, str_nbytes, error);
		memcpy((void *) data, (const void *) buff1, str_nbytes);
		data += str_nbytes;
		sum_bytes += str_nbytes;

		/* write extended opcode */
		db = DW_LNE_end_sequence;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		/* reset value to original values */
		_dwarf_pro_reg_init(prevline);
		no_lns_copy = 1;
		/* this is set only for end_sequence, so that a
		   dw_lns_copy is not generated */
		break;

	    case DW_LNE_set_address:

		/* first null byte */
		db = 0;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);

		/* write length of extended opcode */
		inst_bytes = sizeof(Dwarf_Ubyte) + upointer_size;
		res =
		    _dwarf_pro_encode_leb128_nm(inst_bytes, &str_nbytes,
						buff1, sizeof(buff1));
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		}
		GET_CHUNK(dbg, elfsectno, data, str_nbytes, error);
		str = buff1;
		/* leb number, no endian issue */
		memcpy((void *) data, (const void *) str, str_nbytes);
		data += str_nbytes;
		sum_bytes += str_nbytes;

		/* write extended opcode */
		db = DW_LNE_set_address;
		GET_CHUNK(dbg, elfsectno, data, upointer_size +
			  sizeof(Dwarf_Ubyte), error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);

		/* reloc for address */
		res = dbg->de_reloc_name(dbg, DEBUG_LINE, sum_bytes,	/* r_offset 
									 */
					 curline->dpl_r_symidx,
					 dwarf_drt_data_reloc,
					 uwordb_size);
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		}

		/* write offset (address) */
		du = curline->dpl_address;
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &du,
				sizeof(du), upointer_size);
		data += upointer_size;
		sum_bytes += upointer_size;
		prevline->dpl_address = curline->dpl_address;
		no_lns_copy = 1;
		break;
	    }
	} else {
	    if (curline->dpl_file != prevline->dpl_file) {
		db = DW_LNS_set_file;
		res =
		    _dwarf_pro_encode_leb128_nm(curline->dpl_file,
						&nbytes, buff1,
						sizeof(buff1));
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		}
		arg = buff1;
		GET_CHUNK(dbg, elfsectno, data,
			  nbytes + sizeof(Dwarf_Ubyte), error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		memcpy((void *) data, (const void *) arg, nbytes);
		data += nbytes;
		sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		prevline->dpl_file = curline->dpl_file;
	    }
	    if (curline->dpl_column != prevline->dpl_column) {
		db = DW_LNS_set_column;
		res = _dwarf_pro_encode_leb128_nm(curline->dpl_column,
						  &nbytes,
						  buff1, sizeof(buff1));
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		}

		arg = buff1;
		GET_CHUNK(dbg, elfsectno, data,
			  nbytes + sizeof(Dwarf_Ubyte), error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		memcpy((void *) data, (const void *) arg, nbytes);
		data += nbytes;
		sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		prevline->dpl_column = curline->dpl_column;
	    }
	    if (curline->dpl_is_stmt != prevline->dpl_is_stmt) {
		db = DW_LNS_negate_stmt;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_is_stmt = curline->dpl_is_stmt;
	    }
	    if (curline->dpl_basic_block == true &&
		prevline->dpl_basic_block == false) {
		db = DW_LNS_set_basic_block;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_basic_block = curline->dpl_basic_block;
	    }
	    addr_adv = curline->dpl_address - prevline->dpl_address;

	    line_adv = (int) (curline->dpl_line - prevline->dpl_line);
	    if ((addr_adv % MIN_INST_LENGTH) != 0) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_WRONG_ADDRESS, -1);
	    }
	    if ((opc = _dwarf_pro_get_opc(addr_adv, line_adv)) > 0) {
		no_lns_copy = 1;
		db = opc;
		GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte),
			  error);
		WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
				sizeof(db), sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_basic_block = false;
		prevline->dpl_address = curline->dpl_address;
		prevline->dpl_line = curline->dpl_line;
	    } else {
		if (addr_adv > 0) {
		    db = DW_LNS_advance_pc;
		    res =
			_dwarf_pro_encode_leb128_nm(addr_adv /
						    MIN_INST_LENGTH,
						    &nbytes, buff1,
						    sizeof(buff1));
		    if (res != DW_DLV_OK) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		    }

		    arg = buff1;
		    GET_CHUNK(dbg, elfsectno, data,
			      nbytes + sizeof(Dwarf_Ubyte), error);
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &db,
				    sizeof(db), sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    memcpy((void *) data, (const void *) arg, nbytes);
		    data += nbytes + sizeof(Dwarf_Ubyte);
		    sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		    prevline->dpl_basic_block = false;
		    prevline->dpl_address = curline->dpl_address;
		}
		if (line_adv != 0) {
		    db = DW_LNS_advance_line;
		    res = _dwarf_pro_encode_signed_leb128_nm(line_adv,
							     &nbytes,
							     buff1,
							     sizeof
							     (buff1));
		    if (res != DW_DLV_OK) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
		    }

		    arg = buff1;
		    GET_CHUNK(dbg, elfsectno, data,
			      nbytes + sizeof(Dwarf_Ubyte), error);
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &db, sizeof(db),
				    sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    memcpy((void *) data, (const void *) arg, nbytes);
		    data += nbytes + sizeof(Dwarf_Ubyte);
		    sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		    prevline->dpl_basic_block = false;
		    prevline->dpl_line = curline->dpl_line;
		}
	    }
	}			/* ends else for opc != 0 */
	if (no_lns_copy == 0) {	/* if not a special or dw_lne_end_seq
				   generate a matrix line */
	    db = DW_LNS_copy;
	    GET_CHUNK(dbg, elfsectno, data, sizeof(Dwarf_Ubyte), error);
	    WRITE_UNALIGNED(dbg, (void *) data,
			    (const void *) &db,
			    sizeof(db), sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);
	    sum_bytes += sizeof(Dwarf_Ubyte);
	    prevline->dpl_basic_block = false;
	}
	curline = curline->dpl_next;
    }
    _dwarf_p_dealloc(dbg, prevline);

    /* write total length field */
    du = sum_bytes - uwordb_size - extension_size;	/* subtract
							   length field 
							 */
    {
	start_line_sec += extension_size;
	WRITE_UNALIGNED(dbg, (void *) start_line_sec,
			(const void *) &du, sizeof(du), uwordb_size);
    }

    return (int) dbg->de_n_debug_sect;
}

/*---------------------------------------------------------------
	Generate debug_frame section 
---------------------------------------------------------------*/
static int
_dwarf_pro_generate_debugframe(Dwarf_P_Debug dbg, Dwarf_Error * error)
{
    int elfsectno;
    int i;
    int firsttime = 1;
    int pad;			/* pad for padding to align cies and
				   fdes */
    Dwarf_P_Cie curcie;
    Dwarf_P_Fde curfde;
    unsigned char *data;
#ifdef TARG_X8664
    Dwarf_Signed dsw;
#else
    Dwarf_sfixed dsw;
#endif
    Dwarf_Unsigned du;
    Dwarf_Ubyte db;
    long *cie_offs;		/* holds byte offsets for links to
				   fde's */
    unsigned long cie_length;
    int cie_no;
    int uwordb_size = dbg->de_offset_size;
    int extension_size = dbg->de_64bit_extension ? 4 : 0;
    int upointer_size = dbg->de_pointer_size;
    Dwarf_Unsigned cur_off;	/* current offset of written data,
				   held for relocation info */

#ifdef TARG_X8664
    if (generate_fpic_dwarf) {
	upointer_size = 4;
    }
#endif

    elfsectno = dbg->de_elf_sects[DEBUG_FRAME];

    curcie = dbg->de_frame_cies;
    cie_length = 0;
    cur_off = 0;
    cie_offs = (long *)
	_dwarf_p_get_alloc(dbg, sizeof(long) * dbg->de_n_cie);
    if (cie_offs == NULL) {
	DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_OFFS_ALLOC, -1);
    }
    /* generate cie number as we go along */
    cie_no = 1;
    while (curcie) {
	char *code_al;
	int c_bytes;
	char *data_al;
	int d_bytes;
	int res;
	char buff1[ENCODE_SPACE_NEEDED];
	char buff2[ENCODE_SPACE_NEEDED];
	char buff3[ENCODE_SPACE_NEEDED];
	const char *augmentation;
	char *augmented_al;
	long augmented_fields_length;
	int a_bytes;

	if (cie_no != 1) {
	    fprintf (stderr, "Implement CIE %d", cie_no);
	    abort();
	}
	/* store relocation for cie length */
	res = dbg->de_reloc_name(dbg, DEBUG_FRAME,
				 extension_size, /* r_offset */
				 0, 
				 dwarf_drt_none,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}
	/* store relocation for cie id */
	res = dbg->de_reloc_name(dbg, DEBUG_FRAME,
				 extension_size+uwordb_size, /* r_offset */
				 0, 
				 dwarf_drt_none,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}

	res = _dwarf_pro_encode_leb128_nm(curcie->cie_code_align,
					  &c_bytes,
					  buff1, sizeof(buff1));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_OFFS_ALLOC, -1);
	}
	/* Before April 1999, the following was using an unsigned
	   encode. That worked ok even though the decoder used the
	   correct signed leb read, but doing the encode correctly
	   (according to the dwarf spec) saves space in the output file 
	   and is completely compatible.

	   Note the actual stored amount on MIPS was 10 bytes (!) to
	   store the value -4. (hex)fc ffffffff ffffffff 01 The
	   libdwarf consumer consumed all 10 bytes too!

	   old version res =
	   _dwarf_pro_encode_leb128_nm(curcie->cie_data_align,

	   below is corrected signed version. */
	res = _dwarf_pro_encode_signed_leb128_nm(curcie->cie_data_align,
						 &d_bytes,
						 buff2, sizeof(buff2));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_OFFS_ALLOC, -1);
	}
	code_al = buff1;
	data_al = buff2;

	/* get the correct offset */
	if (firsttime) {
	    cie_offs[cie_no - 1] = 0;
	    firsttime = 0;
	} else {
	    cie_offs[cie_no - 1] = cie_offs[cie_no - 2] +
		(long) cie_length + uwordb_size + extension_size;
	}
	cie_no++;
	augmentation = curcie->cie_aug;
	if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0 ||
	    strcmp(augmentation, PIC_DW_CIE_AUGMENTER_STRING_V0) == 0) {
	    if (generate_fpic_dwarf)	// hard-code it now
		augmented_fields_length = 7;
	    else
		augmented_fields_length = generate_m64_dwarf ? 10 : 6;
	    res = _dwarf_pro_encode_leb128_nm(augmented_fields_length,
					      &a_bytes, buff3,
					      sizeof(buff3));
	    augmented_al = buff3;
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_OFFS_ALLOC, -1);
	    }
	    cie_length = uwordb_size +	/* cie_id */
		sizeof(Dwarf_Ubyte) +	/* cie version */
		strlen(curcie->cie_aug) + 1 +	/* augmentation */
		c_bytes +	/* code alignment factor */
		d_bytes +	/* data alignment factor */
		sizeof(Dwarf_Ubyte) +	/* return reg address */
		a_bytes +	/* augmentation length */
		sizeof(Dwarf_Ubyte) +	/* lsda encoding */
		(generate_fpic_dwarf ? sizeof(Dwarf_Ubyte) : 0) +	/* fde encoding */
		curcie->cie_inst_bytes;
	} else {
	    cie_length = uwordb_size +	/* cie_id */
		sizeof(Dwarf_Ubyte) +	/* cie version */
		strlen(curcie->cie_aug) + 1 +	/* augmentation */
		c_bytes + d_bytes + sizeof(Dwarf_Ubyte) +	/* return 
								   reg
								   address 
								 */
		curcie->cie_inst_bytes;
	}
	pad = (int) PADDING(cie_length, upointer_size);
	cie_length += pad;
	GET_CHUNK(dbg, elfsectno, data, cie_length + uwordb_size
		  + extension_size, error);
	if (extension_size) {
	    Dwarf_Unsigned x = DISTINGUISHED_VALUE;

	    WRITE_UNALIGNED(dbg, (void *) data,
			    (const void *) &x,
			    sizeof(x), extension_size);
	    data += extension_size;

	}
	du = cie_length;
	/* total length of cie */
	WRITE_UNALIGNED(dbg, (void *) data,
			(const void *) &du, sizeof(du), uwordb_size);
	data += uwordb_size;

	/* cie-id is a special value. */
	du = DW_CIE_ID;
	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &du,
			sizeof(du), uwordb_size);
	data += uwordb_size;

	db = curcie->cie_version;
	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
			sizeof(db), sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	strcpy((char *) data, curcie->cie_aug);
	data += strlen(curcie->cie_aug) + 1;
	memcpy((void *) data, (const void *) code_al, c_bytes);
	data += c_bytes;
	memcpy((void *) data, (const void *) data_al, d_bytes);
	data += d_bytes;
	db = curcie->cie_ret_reg;
	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
			sizeof(db), sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);

	if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0 ||
	    strcmp(augmentation, PIC_DW_CIE_AUGMENTER_STRING_V0) == 0) {
	    memcpy((void *) data, (const void *) augmented_al, a_bytes);
	    data += a_bytes;
	}
	memcpy((void *) data, (const void *) curcie->cie_inst,
	       curcie->cie_inst_bytes);
	data += curcie->cie_inst_bytes;
	for (i = 0; i < pad; i++) {
	    *data = DW_CFA_nop;
	    data++;
	}
	curcie = curcie->cie_next;
    }
    /* calculate current offset */
    cur_off = cie_offs[cie_no - 2] + cie_length + uwordb_size
	+ extension_size;

    /* write out fde's */
    curfde = dbg->de_frame_fdes;
    while (curfde) {
	Dwarf_P_Frame_Pgm curinst;
	long fde_length;
	int pad;
	Dwarf_P_Cie cie_ptr;
	Dwarf_Word cie_index, index;
	int oet_length, afl_length, res;
	int v0_augmentation = 0;
	unsigned char *fde_start_point;

	char afl_buff[ENCODE_SPACE_NEEDED];

	/* Find the CIE associated with this fde. */
	cie_ptr = dbg->de_frame_cies;
	cie_index = curfde->fde_cie;
	index = 1;		/* The cie_index of the first cie is 1, 
				   not 0. */
	while (cie_ptr && index < cie_index) {
	    cie_ptr = cie_ptr->cie_next;
	    index++;
	}
	if (cie_ptr == NULL) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_NULL, -1);
	}

	if (strcmp(cie_ptr->cie_aug, DW_CIE_AUGMENTER_STRING_V0) == 0 ||
	    strcmp(cie_ptr->cie_aug, PIC_DW_CIE_AUGMENTER_STRING_V0) == 0) {
	    v0_augmentation = 1;
#ifdef TARG_X8664
	    if (generate_fpic_dwarf || !generate_m64_dwarf)
		oet_length = 4;
	    else
		oet_length = 8;
#else
	    oet_length = sizeof(Dwarf_sfixed);
#endif
	    /* encode the length of augmented fields. */
	    res = _dwarf_pro_encode_leb128_nm(oet_length,
					      &afl_length, afl_buff,
					      sizeof(afl_buff));
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_CIE_OFFS_ALLOC, -1);
	    }

	    fde_length = curfde->fde_n_bytes + uwordb_size +	/* cie
								   pointer 
								 */
		upointer_size +	/* initial loc */
		upointer_size +	/* address range */
		afl_length +	/* augmented field length */
		oet_length;	/* exception_table offset */
	} else {
	    fde_length = curfde->fde_n_bytes + uwordb_size +	/* cie
								   pointer 
								 */
		upointer_size +	/* initial loc */
		upointer_size;	/* address range */
	}

#ifndef TARG_X8664
	/* using fde offset, generate DW_AT_MIPS_fde attribute for the 
	   die corresponding to this fde */
	if (_dwarf_pro_add_AT_fde(dbg, curfde->fde_die, cur_off, error)
	    < 0)
	    return -1;
#endif
	/* store relocation for fde length */
	res = dbg->de_reloc_name(dbg, DEBUG_FRAME,
				 cur_off+extension_size, /* r_offset */
				 0, 
				 dwarf_drt_none,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}
	/* cgdwarf will print the CIE to which this FDE belongs (bug 2463) */
	res = dbg->de_reloc_pair(dbg, DEBUG_FRAME,
				 cur_off+extension_size+uwordb_size, /* r_offset */
                                 dbg->de_sect_name_idx[DEBUG_FRAME], 
				 0, 
				 dwarf_drt_cie_label,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
	}

	/* store relocation information for initial location */
	res = dbg->de_reloc_name(dbg, DEBUG_FRAME,
				 cur_off + extension_size + 2 * uwordb_size,	/* r_offset */
				 curfde->fde_r_symidx,
				 dwarf_drt_data_reloc, upointer_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
	}
	res = dbg->de_reloc_pair(dbg,
				 /* DEBUG_ARANGES, */
				 DEBUG_FRAME,
				 cur_off+extension_size+2*uwordb_size +
				 upointer_size, /* r_offset */
				 curfde->fde_r_symidx,
				 curfde->fde_end_symbol,
				 dwarf_drt_first_of_length_pair,
				 upointer_size);
	if (res != DW_DLV_OK) {
	    _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	    return 0;
	}
	/* Store the relocation information for the
	   offset_into_exception_info field, if the offset is valid (0 
	   is a valid offset). */
	if (v0_augmentation &&
	    curfde->fde_offset_into_exception_tables >= 0) {

	    int size;
#ifdef TARG_X8664
	    size = upointer_size;
#else
	    size = sizeof(Dwarf_sfixed);
#endif
	    res = dbg->de_reloc_name(dbg, DEBUG_FRAME,
				     /* r_offset, where in cie this
				        field starts */
				     cur_off + 2 * uwordb_size +
				     extension_size + 2 * upointer_size + afl_length,
				     curfde->fde_exception_table_symbol,
				     dwarf_drt_segment_rel,
				     size);
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_CHUNK_ALLOC, -1);
	    }
	}

	/* adjust for padding */
	pad = (int) PADDING(fde_length, upointer_size);
	fde_length += pad;


	/* write out fde */
	GET_CHUNK(dbg, elfsectno, data, fde_length + uwordb_size +
		  extension_size, error);
	fde_start_point = data;
	du = fde_length;
	{
	    if (extension_size) {
		Dwarf_Word x = DISTINGUISHED_VALUE;

		WRITE_UNALIGNED(dbg, (void *) data,
				(const void *) &x,
				sizeof(x), extension_size);
		data += extension_size;
	    }
	    /* length */
	    WRITE_UNALIGNED(dbg, (void *) data,
			    (const void *) &du,
			    sizeof(du), uwordb_size);
	    data += uwordb_size;

	    /* offset to cie */
	    du = cie_offs[curfde->fde_cie - 1];
	    WRITE_UNALIGNED(dbg, (void *) data,
			    (const void *) &du,
			    sizeof(du), uwordb_size);
	    data += uwordb_size;

	    du = curfde->fde_initloc;
	    WRITE_UNALIGNED(dbg, (void *) data,
			    (const void *) &du,
			    sizeof(du), upointer_size);
	    data += upointer_size;

	    if (dbg->de_reloc_pair &&
		curfde->fde_end_symbol != 0 &&
		curfde->fde_addr_range == 0) {
		/* symbolic reloc, need reloc for length What if we
		   really know the length? If so, should use the other
		   part of 'if'. */
		Dwarf_Unsigned val;

		/* arrange pre-calc so assem text can do .word end -
		   begin + val (gets val from stream) */
		val = curfde->fde_end_symbol_offset -
		    curfde->fde_initloc;
		WRITE_UNALIGNED(dbg, data,
				(const void *) &val,
				sizeof(val), upointer_size);
		data += upointer_size;
	    } else {

		du = curfde->fde_addr_range;
		WRITE_UNALIGNED(dbg, (void *) data,
				(const void *) &du,
				sizeof(du), upointer_size);
		data += upointer_size;
	    }
	}

	if (v0_augmentation) {
	    int size;
#ifdef TARG_X8664
	    size = upointer_size;
#else
	    size = sizeof(Dwarf_sfixed);
#endif
	    /* write the encoded augmented field length. */
	    memcpy((void *) data, (const void *) afl_buff, afl_length);
	    data += afl_length;
	    /* write the offset_into_exception_tables field. */
	    dsw =
		(Dwarf_sfixed) curfde->fde_offset_into_exception_tables;
	    if (dsw == -1)
		dsw = 0;
	    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &dsw,
			    sizeof(dsw), size);
	    data += size;
	}

	curinst = curfde->fde_inst;
	while (curinst) {
	    db = curinst->dfp_opcode;
	    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
			    sizeof(db), sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);
	    if (DW_CFA_advance_loc4 == db) {
		res = dbg->de_reloc_pair(dbg,
					 DEBUG_FRAME,
					 (data-fde_start_point) +
					 cur_off, /* r_offset */
#ifdef TARG_IA64
					 *(unsigned short *)(&curinst->dfp_args[0]),
					 *(unsigned short *)(&curinst->dfp_args[2]),
#else
                                         *(Dwarf_Unsigned *)(curinst->dfp_args),
                                         *(Dwarf_Unsigned *)(curinst->dfp_args
							     + sizeof(Dwarf_Unsigned)),
#endif
					 dwarf_drt_first_of_length_pair,
					 uwordb_size);
		if (res != DW_DLV_OK) {
		    _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
		    return 0;
		}
	    }
	    if (DW_CFA_advance_loc4 == db) {
		data[0] = 0;
		data[1] = 0;
		data[2] = 0;
		data[3] = 0;
	    } else {
		memcpy((void *) data,
		       (const void *) curinst->dfp_args,
		       curinst->dfp_nbytes);
	    }
	    data += curinst->dfp_nbytes;
	    curinst = curinst->dfp_next;
	}
	/* padding */
	for (i = 0; i < pad; i++) {
	    *data = DW_CFA_nop;
	    data++;
	}
	cur_off += fde_length + uwordb_size;
	curfde = curfde->fde_next;
    }

    _dwarf_p_dealloc(dbg, cie_offs);

    return (int) dbg->de_n_debug_sect;
}



/*---------------------------------------------------------------
	Generate debug_info and debug_abbrev sections
---------------------------------------------------------------*/
static int
_dwarf_pro_generate_debuginfo(Dwarf_P_Debug dbg, Dwarf_Error * error)
{
    int elfsectno_of_debug_info;
    int abbrevsectno;
    unsigned char *data;
#ifdef KEY /* Bug 3507 */
    int upointer_size = dbg->de_pointer_size;
    char *module_name;
    /*
     * To generate the DW_AT_import attribute in a DW_TAG_imported_declaration,
     * Kannan doesn't put the value into the "vsp" buffer. Instead, when it
     * comes time to generate the DW_AT_import in cgdwarf.cxx, he fetches the
     * value of DW_AT_name from the "vsp" buffer and emits a ".quad" with a
     * symbol derived from the name. This means that the 4-byte or 8-byte
     * pointer value of DW_AT_import is missing from "vsp", and after each
     * DW_AT_import, the value of "die_off" is an additional 8 bytes too big
     * relative to the actual offset into "vsp". To fix this, whenever
     * "die_off" was used to index into "vsp", I use ADJUSTED_DIE_OFF instead;
     * hopefully I have found and changed all the places.--sjc
     */
    unsigned count_imported_decls = 0;
#   define ADJUSTED_DIE_OFF (die_off - count_imported_decls * upointer_size)
#endif /* KEY Bug 3507 */
    int cu_header_size;
    Dwarf_P_Abbrev curabbrev, abbrev_head, abbrev_tail;
    Dwarf_P_Die curdie;
    Dwarf_P_Die first_child;
    Dwarf_Word dw;
    Dwarf_Unsigned du;
    Dwarf_Half dh;
    Dwarf_Ubyte db;
    Dwarf_Half version;		/* need 2 byte quantity */
    Dwarf_Unsigned die_off;	/* offset of die in debug_info */
    int n_abbrevs;
    int res;

    Dwarf_Small *start_info_sec;

    int uword_size = dbg->de_offset_size;
    int extension_size = dbg->de_64bit_extension ? 4 : 0;

    abbrev_head = abbrev_tail = NULL;
    elfsectno_of_debug_info = dbg->de_elf_sects[DEBUG_INFO];

    /* write cu header */
    cu_header_size = extension_size + uword_size +	/* length of
							   info section 
							 */
	sizeof(Dwarf_Half) +	/* version stamp */
	uword_size +		/* offset into abbrev table */
	sizeof(Dwarf_Ubyte);	/* size of target address */
    GET_CHUNK(dbg, elfsectno_of_debug_info, data, cu_header_size,
	      error);
    start_info_sec = data;
    if (extension_size) {
	du = DISTINGUISHED_VALUE;
	WRITE_UNALIGNED(dbg, (void *) data,
			(const void *) &du, sizeof(du), extension_size);
	data += extension_size;
    }
    du = 0;			/* length of debug_info, not counting
				   this field itself (unknown at this
				   point). */
    WRITE_UNALIGNED(dbg, (void *) data,
		    (const void *) &du, sizeof(du), uword_size);
    data += uword_size;

    version = CURRENT_VERSION_STAMP;	/* assume this length will not 
					   change */
    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &version,
		    sizeof(version), sizeof(Dwarf_Half));
    data += sizeof(Dwarf_Half);

    du = 0;			/* offset into abbrev table, not yet
				   known. */
    WRITE_UNALIGNED(dbg, (void *) data,
		    (const void *) &du, sizeof(du), uword_size);
    data += uword_size;


    db = dbg->de_pointer_size;

    WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
		    sizeof(db), 1);

    /* We have filled the chunk we got with GET_CHUNK. At this point we 
       no longer dare use "data" or "start_info_sec" as a pointer any
       longer except to refer to that first small chunk for the cu
       header. */

    curdie = dbg->de_dies;

    /* create AT_macro_info if appropriate */
    if (dbg->de_first_macinfo != NULL) {
	if (_dwarf_pro_add_AT_macro_info(dbg, curdie, 0, error) < 0)
	    return -1;
    }

    /* create AT_stmt_list attribute if necessary */
    if (dwarf_need_debug_line_section(dbg) == TRUE)
	if (_dwarf_pro_add_AT_stmt_list(dbg, curdie, error) < 0)
	    return -1;

    die_off = cu_header_size;

#if defined(BUILD_OS_DARWIN)
    /* gcc sets this to zero, and Mach-O linker seems to want that too */
#else /* defined(BUILD_OS_DARWIN) */
    /* 
       Relocation for abbrev offset in cu header store relocation
       record in linked list */
    res = dbg->de_reloc_name(dbg, DEBUG_INFO, extension_size + uword_size + sizeof(Dwarf_Half),	/* r_offset 
												 */
			     dbg->de_sect_name_idx[DEBUG_ABBREV],
			     dwarf_drt_data_reloc, uword_size);
    if (res != DW_DLV_OK) {
	DWARF_P_DBG_ERROR(dbg, DW_DLE_REL_ALLOC, -1);
    }
#endif /* defined(BUILD_OS_DARWIN) */

    /* pass 0: only top level dies, add at_sibling attribute to those
       dies with children */
    first_child = curdie->di_child;
    while (first_child && first_child->di_right) {
	if (first_child->di_child)
	    dwarf_add_AT_reference(dbg,
				   first_child,
				   DW_AT_sibling,
				   first_child->di_right, error);
	first_child = first_child->di_right;
    }

    /* pass 1: create abbrev info, get die offsets, calc relocations */
    while (curdie != NULL) {
	int nbytes;
	Dwarf_P_Attribute curattr;
	char *space;
	int res;
	char buff1[ENCODE_SPACE_NEEDED];

	curdie->di_offset = die_off;
	curabbrev = _dwarf_pro_getabbrev(curdie, abbrev_head);
	if (curabbrev == NULL) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	}
	if (abbrev_head == NULL) {
	    n_abbrevs = 1;
	    curabbrev->abb_idx = n_abbrevs;
	    abbrev_tail = abbrev_head = curabbrev;
	} else {
	    /* check if its a new abbreviation, if yes, add to tail */
	    if (curabbrev->abb_idx == 0) {
		n_abbrevs++;
		curabbrev->abb_idx = n_abbrevs;
		abbrev_tail->abb_next = curabbrev;
		abbrev_tail = curabbrev;
	    }
	}
#ifdef KEY /* Bug 3507 */
	    if (curabbrev->abb_tag == DW_TAG_module) {
	      res = dbg->de_reloc_pair(dbg,
				       DEBUG_INFO,
				       ADJUSTED_DIE_OFF, /* r_offset */
				       dbg->de_sect_name_idx[DEBUG_INFO], 
				       0, 
				       dwarf_drt_module,
				       uword_size);	      
	      if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
	      }
	    }
#endif /* KEY Bug 3507 */
	res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_idx,
					  &nbytes,
					  buff1, sizeof(buff1));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	}
	space = _dwarf_p_get_alloc(dbg, nbytes);
	if (space == NULL) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	}
	memcpy(space, buff1, nbytes);
	curdie->di_abbrev = space;
	curdie->di_abbrev_nbytes = nbytes;
	die_off += nbytes;
	curattr = curdie->di_attrs;
#ifdef KEY /* Bug 3507 */
	    // get the module name
	    if (curabbrev->abb_tag == DW_TAG_imported_declaration)
	      module_name = curattr->ar_data;
#endif /* KEY Bug 3507 */
#if defined(BUILD_OS_DARWIN)
        int skip_stmt_list = 0;
#endif /* defined(BUILD_OS_DARWIN) */
	while (curattr) {
	    if (curattr->ar_rel_type != R_MIPS_NONE) {
		switch (curattr->ar_attribute) {
		case DW_AT_stmt_list:
#if defined(BUILD_OS_DARWIN)
		    skip_stmt_list = 1;
#else /* defined(BUILD_OS_DARWIN) */
		    curattr->ar_rel_symidx =
			dbg->de_sect_name_idx[DEBUG_LINE];
#endif /* defined(BUILD_OS_DARWIN) */
		    break;
		case DW_AT_MIPS_fde:
		    curattr->ar_rel_symidx =
			dbg->de_sect_name_idx[DEBUG_FRAME];
		    break;
		case DW_AT_macro_info:
		    curattr->ar_rel_symidx =
			dbg->de_sect_name_idx[DEBUG_MACINFO];
		    break;
		default:
		    break;
		}
#if defined(BUILD_OS_DARWIN)
                if (!skip_stmt_list) {
#endif /* defined(BUILD_OS_DARWIN) */
		res = dbg->de_reloc_name(dbg, DEBUG_INFO,
#ifdef KEY /* Bug 3507 */
		  ADJUSTED_DIE_OFF + curattr->ar_rel_offset,	/* r_offset */
#else /* KEY Bug 3507 */
		  die_off + curattr->ar_rel_offset,	/* r_offset */
#endif /* KEY Bug 3507 */
					 curattr->ar_rel_symidx,
					 dwarf_drt_data_reloc,
					 curattr->ar_reloc_len);

		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg, DW_DLE_REL_ALLOC, -1);
		}
#if defined(BUILD_OS_DARWIN)
		}
#endif /* defined(BUILD_OS_DARWIN) */

	    }
	    die_off += curattr->ar_nbytes;
	    curattr = curattr->ar_next;
	}
#ifdef KEY /* Bug 3507 */
	    if (curabbrev->abb_tag == DW_TAG_imported_declaration) {
	      // Note that we have overloaded the meaning of the size 
	      // of the relocation to be able to look back for the module
	      // name in the vsp in the back-end. The reason this works is 
	      // because pointers could be 32-bit or 64-bit and cgdwarf and 
	      // the library are in synch as far as that info is concerned.
	      // However, we adjust the die offset by pointer size.
	      res = dbg->de_reloc_pair(dbg,
				       DEBUG_INFO,
				       ADJUSTED_DIE_OFF, /* r_offset */
				       dbg->de_sect_name_idx[DEBUG_INFO], 
				       0, 
				       dwarf_drt_imported_declaration,
				       strlen(module_name));	      
	      if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
	      }
	      /* This 4-byte or 8-byte pointer doesn't appear in the "vsp"
	       * array, but is emitted specially in cgdwarf.cxx function
	       * Cg_Dwarf_Output_Asm_Bytes_Sym_Relocs. */
	      die_off += upointer_size;
	      count_imported_decls += 1;
	    }
#endif /* KEY Bug 3507 */
	/* depth first search */
	if (curdie->di_child)
	    curdie = curdie->di_child;
	else {
	    while (curdie != NULL && curdie->di_right == NULL) {
		curdie = curdie->di_parent;
		die_off++;	/* since we are writing a null die at
				   the end of each sibling chain */
	    }
	    if (curdie != NULL)
		curdie = curdie->di_right;
	}
    }

    /* Pass 2: Write out the die information Here 'data' is a
       temporary, one block for each GET_CHUNK.  'data' is overused. */
    curdie = dbg->de_dies;
    while (curdie != NULL) {
	Dwarf_P_Attribute curattr;

	/* index to abbreviation table */
	GET_CHUNK(dbg, elfsectno_of_debug_info,
		  data, curdie->di_abbrev_nbytes, error);

	memcpy((void *) data,
	       (const void *) curdie->di_abbrev,
	       curdie->di_abbrev_nbytes);

	/* Attribute values - need to fill in all form attributes */
	curattr = curdie->di_attrs;
	while (curattr) {
	    GET_CHUNK(dbg, elfsectno_of_debug_info, data,
		      (unsigned long) curattr->ar_nbytes, error);
	    switch (curattr->ar_attribute_form) {
	    case DW_FORM_ref1:
		{
		    if (curattr->ar_ref_die->di_offset >
			(unsigned) 0xff) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_OFFSET_UFLW, -1);
		    }
		    db = curattr->ar_ref_die->di_offset;
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &db,
				    sizeof(db), sizeof(Dwarf_Ubyte));
		    break;
		}
	    case DW_FORM_ref2:
		{
		    if (curattr->ar_ref_die->di_offset >
			(unsigned) 0xffff) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_OFFSET_UFLW, -1);
		    }
		    dh = curattr->ar_ref_die->di_offset;
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &dh,
				    sizeof(dh), sizeof(Dwarf_Half));
		    break;
		}
	    case DW_FORM_ref_addr:
		{
		    du = curattr->ar_ref_die->di_offset;
		    {
			/* ref to offset of die */
			WRITE_UNALIGNED(dbg, (void *) data,
					(const void *) &du,
					sizeof(du), uword_size);
		    }
		    break;

		}
	    case DW_FORM_ref4:
		{
		    if (curattr->ar_ref_die->di_offset >
			(unsigned) 0xffffffff) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_OFFSET_UFLW, -1);
		    }
		    dw = (Dwarf_Word) curattr->ar_ref_die->di_offset;
		    WRITE_UNALIGNED(dbg, (void *) data,
				    (const void *) &dw,
				    sizeof(dw), sizeof(Dwarf_ufixed));
		    break;
		}
	    case DW_FORM_ref8:
		du = curattr->ar_ref_die->di_offset;
		WRITE_UNALIGNED(dbg, (void *) data,
				(const void *) &du,
				sizeof(du), sizeof(Dwarf_Unsigned));
		break;
	    case DW_FORM_ref_udata:
		{		/* unsigned leb128 offset */

		    int nbytes;
		    char buff1[ENCODE_SPACE_NEEDED];

		    res =
			_dwarf_pro_encode_leb128_nm(curattr->
						    ar_ref_die->
						    di_offset, &nbytes,
						    buff1,
						    sizeof(buff1));
		    if (res != DW_DLV_OK) {
			DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
		    }

		    memcpy(data, buff1, nbytes);
		    break;
		}
	    default:
		memcpy((void *) data,
		       (const void *) curattr->ar_data,
		       curattr->ar_nbytes);
		break;
	    }
	    curattr = curattr->ar_next;
	}

	/* depth first search */
	if (curdie->di_child)
	    curdie = curdie->di_child;
	else {
	    while (curdie != NULL && curdie->di_right == NULL) {
		GET_CHUNK(dbg, elfsectno_of_debug_info, data, 1, error);
		*data = '\0';
		curdie = curdie->di_parent;
	    }
	    if (curdie != NULL)
		curdie = curdie->di_right;
	}
    }

    /* Write out debug_info size */
    /* Dont include length field or extension bytes */
    du = die_off - uword_size - extension_size;
    WRITE_UNALIGNED(dbg, (void *) (start_info_sec + extension_size),
		    (const void *) &du, sizeof(du), uword_size);


    data = 0;			/* Emphasise not usable now */

    /* Write out debug_abbrev section */
    abbrevsectno = dbg->de_elf_sects[DEBUG_ABBREV];

    curabbrev = abbrev_head;
    while (curabbrev) {
	char *val;
	int nbytes;
	int idx;
	int res;
	char buff1[ENCODE_SPACE_NEEDED];

	res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_idx, &nbytes,
					  buff1, sizeof(buff1));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	}

	GET_CHUNK(dbg, abbrevsectno, data, nbytes, error);
	val = buff1;
	memcpy((void *) data, (const void *) val, nbytes);
	res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_tag, &nbytes,
					  buff1, sizeof(buff1));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	}
	val = buff1;
	GET_CHUNK(dbg, abbrevsectno, data, nbytes, error);
	memcpy((void *) data, (const void *) val, nbytes);
	db = curabbrev->abb_children;
	GET_CHUNK(dbg, abbrevsectno, data, sizeof(Dwarf_Ubyte), error);
	WRITE_UNALIGNED(dbg, (void *) data, (const void *) &db,
			sizeof(db), sizeof(Dwarf_Ubyte));

	/* add attributes and forms */
	for (idx = 0; idx < curabbrev->abb_n_attr; idx++) {
	    res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_attrs[idx],
					      &nbytes,
					      buff1, sizeof(buff1));
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	    }
	    val = buff1;
	    GET_CHUNK(dbg, abbrevsectno, data, nbytes, error);
	    memcpy((void *) data, (const void *) val, nbytes);
	    res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_forms[idx],
					      &nbytes,
					      buff1, sizeof(buff1));
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg, DW_DLE_ABBREV_ALLOC, -1);
	    }
	    val = buff1;
	    GET_CHUNK(dbg, abbrevsectno, data, nbytes, error);
	    memcpy((void *) data, (const void *) val, nbytes);
	}
#ifndef KEY
	    GET_CHUNK(dbg,abbrevsectno,data,2,error);	/* two zeros, 
					for last entry */
	    *data = 0;
	    data++;
	    *data = 0;
#else
	    // Bug 3507
	    if (curabbrev->abb_tag == DW_TAG_imported_declaration) {
	      res = _dwarf_pro_encode_leb128_nm(DW_AT_import,
						&nbytes,
						buff1,sizeof(buff1));
	      if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
	      }
	      val = buff1;
	      GET_CHUNK(dbg,abbrevsectno,data,nbytes,error);
	      memcpy((void *)data,(const void *)val, nbytes);
	      res = _dwarf_pro_encode_leb128_nm(DW_FORM_ref_addr,
						&nbytes,
						buff1,sizeof(buff1));
	      if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
	      }
	      val = buff1;
	      GET_CHUNK(dbg,abbrevsectno,data,nbytes,error);
	      memcpy((void *)data,(const void *)val, nbytes);
	    }
            // Bug 294
            // Emit an extra 0x00 at the end of all abbrev section entries.
            // This is necessary for compiling multiple (C) files. If not for 
	    // this, the Number TAG at the beginning of a new compile unit would
	    // be eaten away by the linker. Fortran compilation always link up 
	    // with some external file or some library and will always crash 
	    // TotalView. Gdb would not crash on this bug though.
            if (curabbrev->abb_next) {
              GET_CHUNK(dbg,abbrevsectno,data,2,error);	/* two zeros, 
							   for last entry */
              *data = 0;
	      data++;
	      *data = 0;
            } else {
              GET_CHUNK(dbg,abbrevsectno,data,3,error);	/* two zeros, 
							   for last entry + 
							   one for end of file*/
              *data = 0;
	      data++;
	      *data = 0;
	      data++;
	      *data = 0;
            }
#endif

	curabbrev = curabbrev->abb_next;
    }

    GET_CHUNK(dbg,abbrevsectno,data,1,error);       /* one zero, 
                        for end of cu, see dwarf2 sec 7.5.3 */
    *data = 0;
  

    return (int) dbg->de_n_debug_sect;
}


/*---------------------------------------------------------------------
	Get a buffer of section data. 
	section_idx is the elf-section number that this data applies to. 
	length shows length of returned data 
----------------------------------------------------------------------*/
 /*ARGSUSED*/			/* pretend all args used */
    Dwarf_Ptr
dwarf_get_section_bytes(Dwarf_P_Debug dbg,
			Dwarf_Signed dwarf_section,
			Dwarf_Signed * section_idx,
			Dwarf_Unsigned * length, Dwarf_Error * error)
{
    Dwarf_Ptr buf;

    if (dbg->de_version_magic_number != PRO_VERSION_MAGIC) {
	DWARF_P_DBG_ERROR(dbg, DW_DLE_IA, NULL);
    }

    if (dbg->de_debug_sects == 0) {
	/* no more data !! */
	return NULL;
    }
    if (dbg->de_debug_sects->ds_elf_sect_no == MAGIC_SECT_NO) {
	/* no data ever entered !! */
	return NULL;
    }
    *section_idx = dbg->de_debug_sects->ds_elf_sect_no;
    *length = dbg->de_debug_sects->ds_nbytes;

    buf = (Dwarf_Ptr *) dbg->de_debug_sects->ds_data;

    dbg->de_debug_sects = dbg->de_debug_sects->ds_next;

    /* We may want to call the section stuff more than once: see
       dwarf_reset_section_bytes() do not do: dbg->de_n_debug_sect--; */

    return buf;
}

/*
	No errors possible.
*/
void
dwarf_reset_section_bytes(Dwarf_P_Debug dbg)
{
    dbg->de_debug_sects = dbg->de_first_debug_sect;
    /* No need to reset; commented out decrement. dbg->de_n_debug_sect
       = ???; */
    dbg->de_reloc_next_to_return = 0;
}

/*
    Storage handler. Gets either a new chunk of memory, or
    a pointer in existing memory, from the linked list attached
    to dbg at de_debug_sects, depending on size of nbytes

    Assume dbg not null, checked in top level routine 

    Returns a pointer to the allocated buffer space for the
    lib to fill in,  predincrements next-to-use count so the
    space requested is already counted 'used' 
    when this returns (ie, reserved).

*/
Dwarf_Small *
_dwarf_pro_buffer(Dwarf_P_Debug dbg,
		  int elfsectno, unsigned long nbytes)
{
    Dwarf_P_Section_Data cursect;


    cursect = dbg->de_current_active_section;
    /* By using MAGIC_SECT_NO we allow the following MAGIC_SECT_NO must 
       not match any legit section number. test to have just two
       clauses (no NULL pointer test) See dwarf_producer_init(). */
    if ((cursect->ds_elf_sect_no != elfsectno) ||
	((cursect->ds_nbytes + nbytes) > cursect->ds_orig_alloc)
	) {

	/* Either the elf section has changed or there is not enough
	   space in the current section.

	   Create a new Dwarf_P_Section_Data_s for the chunk. and have
	   space 'on the end' for the buffer itself so we just do one
	   malloc (not two).

	 */
	unsigned long space = nbytes;

	if (nbytes < CHUNK_SIZE)
	    space = CHUNK_SIZE;

	cursect = (Dwarf_P_Section_Data)
	    _dwarf_p_get_alloc(dbg,
			       sizeof(struct Dwarf_P_Section_Data_s)
			       + space);


	if (cursect == NULL)
	    return (NULL);

	/* _dwarf_p_get_alloc zeroes the space... */

	cursect->ds_data = (char *) cursect +
	    sizeof(struct Dwarf_P_Section_Data_s);
	cursect->ds_orig_alloc = space;
	cursect->ds_elf_sect_no = elfsectno;
	cursect->ds_nbytes = nbytes;	/* reserve this number of bytes 
					   of space for caller to fill
					   in */

	/* Now link on the end of the list, and mark this one as the
	   current one */

	if (dbg->de_debug_sects->ds_elf_sect_no == MAGIC_SECT_NO) {
	    /* the only entry is the special one for 'no entry' so
	       delete that phony one while adding this initial real
	       one. */
	    dbg->de_debug_sects = cursect;
	    dbg->de_current_active_section = cursect;
	    dbg->de_first_debug_sect = cursect;
	} else {
	    dbg->de_current_active_section->ds_next = cursect;
	    dbg->de_current_active_section = cursect;
	}
	dbg->de_n_debug_sect++;

	return ((Dwarf_Small *) cursect->ds_data);
    }

    /* There is enough space in the current buffer */
    {
	Dwarf_Small *space_for_caller = (Dwarf_Small *)
	    (cursect->ds_data + cursect->ds_nbytes);

	cursect->ds_nbytes += nbytes;
	return space_for_caller;
    }
}


/*------------------------------------------------------------
	Given address advance and line advance, it gives 
	either special opcode, or a number < 0
------------------------------------------------------------*/
static int
_dwarf_pro_get_opc(Dwarf_Unsigned addr_adv, int line_adv)
{
    int opc;

    addr_adv = addr_adv / MIN_INST_LENGTH;
    if (line_adv == 0 && addr_adv == 0)
	return OPC_INCS_ZERO;
    if (line_adv >= LINE_BASE && line_adv < LINE_BASE + LINE_RANGE) {
	opc =
	    (line_adv - LINE_BASE) + (addr_adv * LINE_RANGE) +
	    OPCODE_BASE;
	if (opc > 255)
	    return OPC_OUT_OF_RANGE;
	return opc;
    } else
	return LINE_OUT_OF_RANGE;
}

/*-----------------------------------------------------------------------
	Handles abbreviations. It takes a die, searches through 
	current list of abbreviations for matching one. If it
	finds one, it returns a pointer to it, and if it doesnt, 
	it returns a new one. Upto the user of this function to 
	link it up to the abbreviation head. If its a new one,
	abb_idx has 0.
-----------------------------------------------------------------------*/
static Dwarf_P_Abbrev
_dwarf_pro_getabbrev(Dwarf_P_Die die, Dwarf_P_Abbrev head)
{
    Dwarf_P_Abbrev curabbrev;
    Dwarf_P_Attribute curattr;
    int res1;
    int nattrs;
    int match;
    Dwarf_ufixed *forms = 0;
    Dwarf_ufixed *attrs = 0;

    curabbrev = head;
    while (curabbrev) {
	if ((die->di_tag == curabbrev->abb_tag) &&
	    ((die->di_child != NULL &&
	      curabbrev->abb_children == DW_CHILDREN_yes) ||
	     (die->di_child == NULL &&
	      curabbrev->abb_children == DW_CHILDREN_no)) &&
	    (die->di_n_attr == curabbrev->abb_n_attr)) {

	    /* There is a chance of a match. */
	    curattr = die->di_attrs;
	    match = 1;		/* Assume match found. */
	    while (match && curattr) {
		res1 = _dwarf_pro_match_attr(curattr,
					     curabbrev,
					     (int) curabbrev->
					     abb_n_attr);
		if (res1 == 0)
		    match = 0;
		curattr = curattr->ar_next;
	    }
	    if (match == 1)
		return curabbrev;
	}
	curabbrev = curabbrev->abb_next;
    }

    /* no match, create new abbreviation */
    if (die->di_n_attr != 0) {
	forms = (Dwarf_ufixed *)
	    _dwarf_p_get_alloc(NULL,
			       sizeof(Dwarf_ufixed) * die->di_n_attr);
	if (forms == NULL)
	    return NULL;
	attrs = (Dwarf_ufixed *)
	    _dwarf_p_get_alloc(NULL,
			       sizeof(Dwarf_ufixed) * die->di_n_attr);
	if (attrs == NULL)
	    return NULL;
    }
    nattrs = 0;
    curattr = die->di_attrs;
    while (curattr) {
	attrs[nattrs] = curattr->ar_attribute;
	forms[nattrs] = curattr->ar_attribute_form;
	nattrs++;
	curattr = curattr->ar_next;
    }

    curabbrev = (Dwarf_P_Abbrev)
	_dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Abbrev_s));
    if (curabbrev == NULL)
	return NULL;

    if (die->di_child == NULL)
	curabbrev->abb_children = DW_CHILDREN_no;
    else
	curabbrev->abb_children = DW_CHILDREN_yes;
    curabbrev->abb_tag = die->di_tag;
    curabbrev->abb_attrs = attrs;
    curabbrev->abb_forms = forms;
    curabbrev->abb_n_attr = die->di_n_attr;
    curabbrev->abb_idx = 0;
    curabbrev->abb_next = NULL;

    return curabbrev;
}

/*------------------------------------------------------------------
	Tries to see if given attribute and form combination 
	exists in the given abbreviation
-------------------------------------------------------------------*/
static int
_dwarf_pro_match_attr(Dwarf_P_Attribute attr,
		      Dwarf_P_Abbrev abbrev, int no_attr)
{
    int i;
    int found = 0;

    for (i = 0; i < no_attr; i++) {
	if (attr->ar_attribute == abbrev->abb_attrs[i] &&
	    attr->ar_attribute_form == abbrev->abb_forms[i]) {
	    found = 1;
	    break;
	}
    }
    return found;
}

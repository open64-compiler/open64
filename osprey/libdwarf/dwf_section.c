/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.

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

int
_dwf_pro_generate_ehframe(Dwarf_P_Debug dbg, Dwarf_Error *error)
{
    int elfsectno;
    int i;
    int firsttime = 1;
#ifdef TARG_IA64
    int  pad;	/* pad for padding to align cies and fdes */
#endif
    Dwarf_P_Cie curcie;
    Dwarf_P_Fde curfde;
    unsigned char *data;
    Dwarf_Signed dsw;
    Dwarf_Ubyte Personality_Format;
    Dwarf_Unsigned du;
    Dwarf_Ubyte db;
    long *cie_offs;		/* holds byte offsets for links to fde's */
    unsigned long cie_length;
    int cie_no;
    int uwordb_size = dbg->de_offset_size;
    int extension_size =  dbg->de_64bit_extension? 4:0;
    int upointer_size = dbg->de_pointer_size;
    Dwarf_Unsigned cur_off;		/* current offset of written
					   data, held for relocation info */

    if (generate_fpic_dwarf)
    {
	upointer_size = 4;
	Personality_Format = 0x9b;
    } else {
	Personality_Format = 0;
    }

    elfsectno = dbg->de_elf_sects[EH_FRAME];

    curcie = dbg->de_eh_frame_cies;
    cie_length = 0;
    cur_off = 0;
    cie_offs = (long *)
	_dwarf_p_get_alloc(dbg, sizeof(long)*dbg->de_eh_n_cie);
    if (cie_offs == NULL) {
	DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
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
	int  a_bytes;

	Dwarf_Unsigned personality = curcie->cie_personality;
	int personality_length = personality ? upointer_size : 0;

	if (cie_no != 1) {
	    fprintf (stderr,"Implement multiple CIE's");
	    abort();
	}
	// store relocation for cie length
	res = dbg->de_reloc_name(dbg, EH_FRAME,
				 extension_size, /* r_offset */
				 0,
#ifdef TARG_IA64
				 dwarf_drt_none,
#else
				 dwarf_drt_cie_begin,
#endif
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}
	// store relocation for cie id
	res = dbg->de_reloc_name(dbg, EH_FRAME,
				 extension_size+uwordb_size, /* r_offset */
				 0,
				 dwarf_drt_none,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}
	res = _dwarf_pro_encode_leb128_nm(curcie->cie_code_align,
					  &c_bytes,
					  buff1,sizeof(buff1));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	}
	/* Before April 1999, the following was using
	   an unsigned encode.
	   That worked ok even though the decoder used
	   the correct signed leb read, but doing
	   the encode correctly (according to the dwarf
	   spec) saves space  in
	   the output file and is completely compatible.

	   Note the actual stored amount on MIPS was
	   10 bytes (!) to store the value -4.
	   (hex)fc ffffffff ffffffff 01
	   The libdwarf consumer consumed all 10 bytes too!

	   old version
	   res = _dwarf_pro_encode_leb128_nm(curcie->cie_data_align,

	   below is corrected signed version.
	*/
	res = _dwarf_pro_encode_signed_leb128_nm(curcie->cie_data_align,
						 &d_bytes,
						 buff2,sizeof(buff2));
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	}
	code_al = buff1;
	data_al = buff2;

	/* get the correct offset */
	if (firsttime) {
	    cie_offs[cie_no-1] = 0  ;
	    firsttime = 0;
	} else {
	    cie_offs[cie_no-1] = cie_offs[cie_no-2] +
		(long)cie_length +
		uwordb_size + extension_size;
	}
	cie_no++;
	augmentation = curcie->cie_aug;
	if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0
	    || !strcmp(augmentation, PIC_DW_CIE_AUGMENTER_STRING_V0)
	    // Bug 7278 - implement "zR" CFA augmentation for non-C++ code.
	    || !strcmp(augmentation,
		       PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0)
	    ) {
	    if (!strcmp(augmentation, 
			PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0)) {
		augmented_fields_length = 1;
		Personality_Format = 0x1b;
	    }
	    else if (generate_fpic_dwarf)	// hard-code it now
		augmented_fields_length = 7;
	    else
		augmented_fields_length = generate_m64_dwarf ? 10 : 6;
	    res = _dwarf_pro_encode_leb128_nm(augmented_fields_length,
					      &a_bytes, buff3,
					      sizeof(buff3));
	    augmented_al = buff3;
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	    }
	    cie_length =
		uwordb_size +   /* cie_id */
		sizeof(Dwarf_Ubyte) +		/* cie version */
		strlen(curcie->cie_aug)+1  + 	/* augmentation */
		c_bytes +	/* code alignment factor */
		d_bytes +      /* data alignment factor */
		sizeof(Dwarf_Ubyte) +	  /* return reg address */
		a_bytes +      /* augmentation length   */
		sizeof(Dwarf_Ubyte) +	  /* personality format */
		personality_length +	/* personality routine */
		sizeof(Dwarf_Ubyte) +	  /* lsda encoding */
		(generate_fpic_dwarf ? sizeof(Dwarf_Ubyte) : 0) + /* fde encoding */
		curcie->cie_inst_bytes;
	}
	else {
	    cie_length =
		uwordb_size + 	/* cie_id */
		sizeof(Dwarf_Ubyte) +		/* cie version */
		strlen(curcie->cie_aug)+1  + 	/* augmentation */
		c_bytes +
		d_bytes +
		sizeof(Dwarf_Ubyte) +	/* return reg address */
		curcie->cie_inst_bytes;
	}
#ifdef TARG_IA64
	pad = (int)PADDING(cie_length, upointer_size);
	cie_length += pad;
#endif
	if (personality) {
	    res = dbg->de_reloc_name(dbg, EH_FRAME,
				     extension_size+2*uwordb_size+
				     3*sizeof(Dwarf_Ubyte)+
				     (strlen(curcie->cie_aug)+1)+
				     c_bytes+d_bytes+a_bytes, /* r_offset */
				     personality,
				     dwarf_drt_data_reloc_by_str_id,
				     upointer_size);
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	    }
	}
	GET_CHUNK(dbg,elfsectno,data,cie_length+uwordb_size
		  +extension_size, error);
	if (extension_size) {
	    Dwarf_Unsigned x = DISTINGUISHED_VALUE;
	    WRITE_UNALIGNED(dbg,(void *)data,
			    (const void *)&x,
			    sizeof(x), extension_size);
	    data += extension_size;

	}
	du = cie_length;
	/* total length of cie */
#ifdef TARG_IA64
	WRITE_UNALIGNED(dbg,(void *)data,
			(const void *)&du,
			sizeof(du), uwordb_size);
#endif
        /* the length will be computed using 'begin' and 'end' labels. */
	data += uwordb_size;

	/*cie-id is a special value. */
#ifdef KEY
	// Bug 5357 - g++ uses 0 as the CIE_ID for eh_frame section and
	// 0xffffffff as the CIE_ID in the debug_frame section.
	du = 0x0LL;
#else
	du = DW_CIE_ID;
#endif
	WRITE_UNALIGNED(dbg,(void *)data, (const void *)&du,
			sizeof(du), uwordb_size);
	data += uwordb_size;

	db = curcie->cie_version;
	WRITE_UNALIGNED(dbg,(void *)data, (const void *)&db,
			sizeof(db),sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	strcpy((char *)data, curcie->cie_aug);
	data += strlen(curcie->cie_aug)+1;
	memcpy((void *)data, (const void *)code_al, c_bytes);
	data += c_bytes;
	memcpy((void *)data, (const void *)data_al, d_bytes);
	data += d_bytes;
	db = curcie->cie_ret_reg;
	WRITE_UNALIGNED(dbg,(void *)data, (const void *)&db,
			sizeof(db),sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);

	if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0
	    || !strcmp(augmentation, PIC_DW_CIE_AUGMENTER_STRING_V0)
	    // Bug 7278 - implement "zR" CFA augmentation for non-C++ code.
	    || !strcmp(augmentation, 
		       PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0)
	    ) {
	    memcpy((void *)data, (const void *) augmented_al, a_bytes);
	    data += a_bytes;
	}

	if (personality) {
        // bug 9177: Emit personality format only if there is
        // personality information.
        //
        // personality format
        db = Personality_Format;
        WRITE_UNALIGNED(dbg, (void *)data, (const void *)&db,
                sizeof(db), sizeof(Dwarf_Ubyte));
        data += sizeof(Dwarf_Ubyte);
        // personality routine offset
        Dwarf_Unsigned p = 0;
        WRITE_UNALIGNED(dbg, (void *)data, (const void *)&p,
                sizeof(p), upointer_size);
        data += upointer_size;

        if (generate_fpic_dwarf) {
        p = 0x1b;
        }
        // lsda encoding
        WRITE_UNALIGNED(dbg, (void *)data, (const void *)&p,
                sizeof(p), sizeof(Dwarf_Ubyte));
        data += sizeof(Dwarf_Ubyte);
	}
	if (generate_fpic_dwarf) {
	    // FDE encoding should be added for all languages (bug 12323).
	    Dwarf_Unsigned p = 0x1b;
	    WRITE_UNALIGNED(dbg, (void *)data, (const void *)&p,
			    sizeof(p), sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);
	}
	memcpy((void *)data, (const void *)curcie->cie_inst, curcie->cie_inst_bytes);
	data += curcie->cie_inst_bytes;
#ifdef TARG_IA64
	for (i = 0 ; i < pad ; i++) {
	    *data = DW_CFA_nop;
	    data++;
	}
#endif
	curcie = curcie->cie_next;
    }
    /* calculate current offset */
    cur_off = cie_offs[cie_no-2] + cie_length + uwordb_size
	+ extension_size;

    /* write out fde's */
    curfde = dbg->de_eh_frame_fdes;
    while (curfde) {
	Dwarf_P_Frame_Pgm curinst;
	long fde_length;
#ifdef TARG_IA64
	int pad;
#endif
	Dwarf_P_Cie cie_ptr;
	Dwarf_Word  cie_index, index;
	int oet_length, afl_length, res;
	int v0_augmentation = 0;
	int pic_augmentation = 0;
	unsigned char *fde_start_point;

	char afl_buff[ENCODE_SPACE_NEEDED];

	/* Find the CIE associated with this fde. */
	cie_ptr = dbg->de_eh_frame_cies;
	cie_index = curfde->fde_cie;
	index = 1; /* The cie_index of the first cie is 1, not 0. */
	while (cie_ptr && index < cie_index) {
	    cie_ptr = cie_ptr->cie_next;
	    index++;
	}
	if (cie_ptr == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_NULL,-1);
	}

	if (strcmp(cie_ptr->cie_aug, DW_CIE_AUGMENTER_STRING_V0) == 0
	    || !strcmp(cie_ptr->cie_aug, PIC_DW_CIE_AUGMENTER_STRING_V0)
	    ) {
	    v0_augmentation = 1;
	    if (generate_fpic_dwarf || !generate_m64_dwarf)
		oet_length = 4;
	    else
		oet_length = 8;
	    /* encode the length of augmented fields. */
	    res = _dwarf_pro_encode_leb128_nm(
		oet_length,
		&afl_length, afl_buff,
		sizeof(afl_buff));
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	    }

	    fde_length = curfde->fde_n_bytes +
		uwordb_size +		/* cie pointer */
		upointer_size + 	/* initial loc */
		upointer_size +	/* address range */
		afl_length        +   /* augmented field length */
		oet_length;   /* exception_table offset */
	}
	else if (!strcmp(cie_ptr->cie_aug,
	                 PIC_NONCPLUS_DW_CIE_AUGMENTER_STRING_V0) &&
	         generate_fpic_dwarf) {
	    // "zR" augmentation for non-C++ PIC, insert augmentation
 	    // size (bug 12323).
    	    pic_augmentation = 1;
    	    oet_length = 0;
    	    /* encode the length of augmented fields. */
    	    res = _dwarf_pro_encode_leb128_nm(
   	    		oet_length,
			&afl_length, afl_buff,
			sizeof(afl_buff));
	    if (res != DW_DLV_OK) {
 		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	    }

	    fde_length = curfde->fde_n_bytes +
			uwordb_size +      /* cie pointer */
			upointer_size +    /* initial loc */
			upointer_size +    /* address range */
			afl_length;        /* augmented field length */
	}
	else {
	    fde_length = curfde->fde_n_bytes +
		uwordb_size +		/* cie pointer */
		upointer_size + 	/* initial loc */
		upointer_size;		/* address range */
	}

	// store relocation for fde length
	res = dbg->de_reloc_name(dbg, EH_FRAME,
				 cur_off+extension_size, /* r_offset */
				 0,
#ifdef TARG_IA64
				 dwarf_drt_none,
#else
                                 dwarf_drt_fde_begin,
#endif
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}
	// store relocation for cie-offset -- currentpos - ciestart
	res = dbg->de_reloc_pair(dbg, EH_FRAME,
				 cur_off+extension_size+uwordb_size, /* r_offset */
				 dbg->de_sect_name_idx[EH_FRAME],
				 0,
				 dwarf_drt_first_of_length_pair_create_second,
				 uwordb_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}

	/* store relocation information for initial location */
	res = dbg->de_reloc_name(dbg,
				 EH_FRAME,
				 cur_off+extension_size+2*uwordb_size, /* r_offset */
				 curfde->fde_r_symidx,
				 dwarf_drt_data_reloc,
				 upointer_size);
	if (res != DW_DLV_OK) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	}

	res = dbg->de_reloc_pair(dbg,
				 /* DEBUG_ARANGES, */
				 EH_FRAME,
				 cur_off +extension_size+2*uwordb_size +
				 upointer_size, /* r_offset */
				 curfde->fde_r_symidx,
				 curfde->fde_end_symbol,
				 dwarf_drt_first_of_length_pair,
				 upointer_size);
	if (res != DW_DLV_OK) {
	    {_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}
	}
	/* Store the relocation information for the
	   offset_into_exception_info field, if the offset is
	   valid (0 is a valid offset). */
	if (v0_augmentation &&
	    curfde->fde_offset_into_exception_tables >= 0) {

	    res = dbg->de_reloc_name(dbg,
				     EH_FRAME,

				     /* r_offset, where in cie
					this field starts */
				     cur_off+2*uwordb_size+
				     extension_size+2*upointer_size + afl_length,

				     curfde->fde_exception_table_symbol,
				     dwarf_drt_segment_rel,
				     upointer_size);
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
	    }
	}

#ifdef TARG_IA64
	/* adjust for padding */
	pad = (int)PADDING(fde_length, upointer_size);
	fde_length += pad;
#endif

	/* write out fde */
	GET_CHUNK(dbg,elfsectno,data,fde_length+uwordb_size +
		  extension_size, error);
	fde_start_point = data;
	du = fde_length;
	{
	    if (extension_size) {
		Dwarf_Word x = DISTINGUISHED_VALUE;
		WRITE_UNALIGNED(dbg,(void *)data,
				(const void *)&x,
				sizeof(x), extension_size);
		data += extension_size;
	    }
	    /* length */
#ifdef TARG_IA64
	    WRITE_UNALIGNED(dbg,(void *)data,
			    (const void *)&du,
			    sizeof(du), uwordb_size);
#endif
            /* the FDE length will be computed using 'begin/end' labels. */
	    data += uwordb_size;

	    /* offset to cie */
	    du = cie_offs[curfde->fde_cie-1];
	    WRITE_UNALIGNED(dbg,(void *)data,
			    (const void *)&du,
			    sizeof(du),uwordb_size);
	    data += uwordb_size;

	    du = curfde->fde_initloc;
	    WRITE_UNALIGNED(dbg,(void *)data,
			    (const void *)&du,
			    sizeof(du),upointer_size);
	    data += upointer_size;

	    if (dbg->de_reloc_pair &&
		curfde->fde_end_symbol != 0 &&
		curfde->fde_addr_range == 0) {
		/* symbolic reloc, need reloc for length
		   What if we really know the length?
		   If so, should use the other part of 'if'.
		*/
		Dwarf_Unsigned val;

		/*    arrange pre-calc so assem text can do
		      .word end - begin + val (gets val from stream)
		*/
		val = curfde->fde_end_symbol_offset -
		    curfde->fde_initloc;
		WRITE_UNALIGNED(dbg,data,
				(const void *)&val,
				sizeof(val), upointer_size);
		data += upointer_size;
	    } else {

		du = curfde->fde_addr_range;
		WRITE_UNALIGNED(dbg,(void *)data,
				(const void *) &du,
				sizeof(du),upointer_size);
		data += upointer_size;
	    }
	}

	if (v0_augmentation) {
	    /* write the encoded augmented field length. */
	    memcpy((void *)data, (const void *)afl_buff, afl_length);
	    data += afl_length;
	    /* write the offset_into_exception_tables field. */
	    dsw = curfde->fde_offset_into_exception_tables;
	    if (dsw == -1) dsw = 0;
	    WRITE_UNALIGNED(dbg,(void *)data, (const void *)&dsw,
			    sizeof(dsw), upointer_size);
	    data += upointer_size;
	}
	else if (pic_augmentation) {
	    /* write the encoded augmented field length. */
	    memcpy((void *)data, (const void *)afl_buff, afl_length);
    	    data += afl_length;
    	}
	curinst = curfde->fde_inst;
	while (curinst) {
	    db = curinst->dfp_opcode;
	    WRITE_UNALIGNED(dbg,(void *)data, (const void *)&db,
			    sizeof(db), sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);
	    if (DW_CFA_advance_loc4 == db) {
		res = dbg->de_reloc_pair(dbg,
					 EH_FRAME,
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
		    {_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}
		}
	    }
	    if (DW_CFA_advance_loc4 == db) {
		data[0] = 0;
		data[1] = 0;
		data[2] = 0;
		data[3] = 0;
	    } else
		memcpy((void *)data,
		       (const void *)curinst->dfp_args,
		       curinst->dfp_nbytes);
	    data += curinst->dfp_nbytes;
	    curinst = curinst->dfp_next;
	}
#ifdef TARG_IA64
	/* padding */
	for (i = 0 ; i < pad ; i++) {
	    *data = DW_CFA_nop;
	    data++;
	}
#endif
	cur_off += fde_length + uwordb_size;
	curfde = curfde->fde_next;
    }

    _dwarf_p_dealloc(dbg, cie_offs);

    return (int)dbg->de_n_debug_sect;
}

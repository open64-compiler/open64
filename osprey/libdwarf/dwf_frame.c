/*

  Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  You should have received a copy of the GNU Lesser General Public
  License along with this program; if not, write the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307,
  USA.

*/

#include "config.h"
#include "config.h"
#include "dwarf_stuff.h"
#include "libdwarfdefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "pro_incl.h"
#include "pro_frame.h"

Dwarf_Bool generate_fpic_dwarf;
Dwarf_Bool generate_m64_dwarf;

Dwarf_Unsigned
dwf_add_frame_cie(Dwarf_P_Debug dbg,
		  const char *augmenter,
		  Dwarf_Small code_align,
		  Dwarf_Small data_align,
		  Dwarf_Small return_reg,
		  Dwarf_Unsigned personality,
		  Dwarf_Bool pic,
		  Dwarf_Bool is_64bit,
		  Dwarf_Ptr init_bytes,
		  Dwarf_Unsigned init_n_bytes,
		  Dwarf_Error * error)
{
    generate_fpic_dwarf = pic;
    generate_m64_dwarf = is_64bit;

    if (personality) {
	fprintf(stderr, "Implement personality %ud\n",
		(unsigned int) personality);
	abort();
    }

    return dwarf_add_frame_cie(dbg, augmenter, code_align, data_align,
			       return_reg,
			       init_bytes,
			       init_n_bytes,
			       error);
}

Dwarf_Unsigned
dwf_add_ehframe_cie(Dwarf_P_Debug dbg,
		    const char *augmenter,
		    Dwarf_Small code_align,
		    Dwarf_Small data_align,
		    Dwarf_Small return_reg,
		    Dwarf_Unsigned personality,
		    Dwarf_Bool     fpic,
		    Dwarf_Bool     is_64bit,
		    Dwarf_Ptr init_bytes,
		    Dwarf_Unsigned init_n_bytes,
		    Dwarf_Error *error)
{
    Dwarf_P_Cie curcie;
    generate_fpic_dwarf = fpic;
    generate_m64_dwarf = is_64bit;

    if (dbg->de_eh_frame_cies == NULL) {
	dbg->de_eh_frame_cies = (Dwarf_P_Cie)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Cie_s));
	if (dbg->de_eh_frame_cies == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_ALLOC,DW_DLV_NOCOUNT);
	}
	curcie = dbg->de_eh_frame_cies;
	dbg->de_eh_n_cie = 1;
	dbg->de_eh_last_cie = curcie;
    }
    else {
	curcie = dbg->de_eh_last_cie;
	curcie->cie_next = (Dwarf_P_Cie)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Cie_s));
	if (curcie->cie_next == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_ALLOC,DW_DLV_NOCOUNT);
	}
	curcie = curcie->cie_next;
	dbg->de_eh_n_cie++;
	dbg->de_eh_last_cie = curcie;
    }
    curcie->cie_version = DW_CIE_VERSION;
    curcie->cie_aug = augmenter;
    curcie->cie_code_align = code_align;
    curcie->cie_data_align = data_align;
    curcie->cie_ret_reg = return_reg;
    curcie->cie_personality = personality;
    curcie->cie_inst = (char *) init_bytes;
    curcie->cie_inst_bytes = (long)init_n_bytes;
    curcie->cie_next = NULL;
    return dbg->de_eh_n_cie;
}

/*ARGSUSED*/ /* pretend all args used */
Dwarf_Unsigned
dwf_add_ehframe_fde(Dwarf_P_Debug dbg,
		    Dwarf_P_Fde fde,
		    Dwarf_P_Die die,
		    Dwarf_Unsigned cie,
		    Dwarf_Unsigned virt_addr,
		    Dwarf_Unsigned code_len,
		    Dwarf_Unsigned  symidx,
		    Dwarf_Error *error)
{
    return dwf_add_ehframe_fde_b(dbg, fde, die, cie, virt_addr,
				 code_len, symidx, 0, 0, error);
}

/*ARGSUSED10*/
Dwarf_Unsigned
dwf_add_ehframe_fde_b(Dwarf_P_Debug dbg,
		      Dwarf_P_Fde fde,
		      Dwarf_P_Die die,
		      Dwarf_Unsigned cie,
		      Dwarf_Unsigned virt_addr,
		      Dwarf_Unsigned code_len,
		      Dwarf_Unsigned symidx,
		      Dwarf_Unsigned symidx_of_end,
		      Dwarf_Addr offset_from_end_sym,
		      Dwarf_Error *error)
{
    Dwarf_P_Fde curfde;

    fde->fde_die = die;
    fde->fde_cie = (long)cie;
    fde->fde_initloc = virt_addr;
    fde->fde_r_symidx = symidx;
    fde->fde_addr_range = code_len;
    fde->fde_offset_into_exception_tables = DW_DLX_NO_EH_OFFSET;
    fde->fde_exception_table_symbol = 0;
    fde->fde_end_symbol_offset = offset_from_end_sym;
    fde->fde_end_symbol = symidx_of_end;

    curfde = dbg->de_eh_last_fde;
    if (curfde == NULL) {
	dbg->de_eh_frame_fdes = fde;
	dbg->de_eh_last_fde = fde;
	dbg->de_eh_n_fde = 1;
    }
    else {
	curfde->fde_next = fde;
	dbg->de_eh_last_fde = fde;
	dbg->de_eh_n_fde++;
    }
    return dbg->de_eh_n_fde;
}

Dwarf_Unsigned
dwf_add_ehframe_info(Dwarf_P_Debug dbg,
		     Dwarf_P_Fde fde,
		     Dwarf_P_Die die,
		     Dwarf_Unsigned cie,
		     Dwarf_Unsigned virt_addr,
		     Dwarf_Unsigned code_len,
		     Dwarf_Unsigned symidx,
		     Dwarf_Signed offset_into_exception_tables,
		     Dwarf_Unsigned exception_table_symbol,
		     Dwarf_Error *error)
{
    return dwf_add_ehframe_info_b(
	dbg,fde,die,cie,virt_addr,
	code_len,
	symidx,
	/* end_symbol */0,
	/* offset_from_end */0,
	offset_into_exception_tables,
	exception_table_symbol,
	error);

}

/*ARGSUSED*/ /* pretend all args used */
Dwarf_Unsigned
dwf_add_ehframe_info_b(Dwarf_P_Debug dbg,
		       Dwarf_P_Fde fde,
		       Dwarf_P_Die die,
		       Dwarf_Unsigned cie,
		       Dwarf_Unsigned virt_addr,
		       Dwarf_Unsigned code_len,
		       Dwarf_Unsigned symidx,
		       Dwarf_Unsigned end_symidx,
		       Dwarf_Unsigned offset_from_end_symbol,
		       Dwarf_Signed offset_into_exception_tables,
		       Dwarf_Unsigned exception_table_symbol,
		       Dwarf_Error *error)
{
    Dwarf_P_Fde curfde;

    fde->fde_die = die;
    fde->fde_cie = (long)cie;
    fde->fde_initloc = virt_addr;
    fde->fde_r_symidx = symidx;
    fde->fde_addr_range = code_len;
    fde->fde_offset_into_exception_tables = offset_into_exception_tables;
    fde->fde_exception_table_symbol = exception_table_symbol;
    fde->fde_end_symbol_offset = offset_from_end_symbol;
    fde->fde_end_symbol = end_symidx;


    curfde = dbg->de_eh_last_fde;
    if (curfde == NULL) {
	dbg->de_eh_frame_fdes = fde;
	dbg->de_eh_last_fde = fde;
	dbg->de_eh_n_fde = 1;
    }
    else {
	curfde->fde_next = fde;
	dbg->de_eh_last_fde = fde;
	dbg->de_eh_n_fde++;
    }
    return dbg->de_eh_n_fde;
}

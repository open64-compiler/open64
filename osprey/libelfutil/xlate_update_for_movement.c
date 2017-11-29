/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/*
   fixxlate.c

   $Revision: 1.1.1.1 $

   xlate section fixer.
   
   The xlate section has addresses in its header and block headers.

   Each of these must be fixed up.

   We should be fixing register info too, but that won't exist
   in a corded binary and we don't allow for rqs-ing a
   pixie-d binary at the moment.

   FIX: handle updating register data (the pc address in theat section
	of the xlate file)

   The caller must do the io and handle any ELF
   issues: this function only handles
   address update within the section.

   When updating a 32bit object, the upper 32 bits of the
	movement argument are ignored.

   We do copies so the caller need not have 
   passed in properly aligned data.

   Updating can only fail if the section is illogical/incomplete
   (corruputed in some way).


*/

#ifndef _LP64
#include <sgidefs.h> /* for types to use. */
#endif /* _LP64 */
#include <stdio.h>
#include "xlateincl.h"

/*
   We do copies so the caller need not have properly aligned data.
   Wasteful but safer.
*/
static int
update_blockheader32(char *curbase,unsigned int len,unsigned int movement)
{
	xlate_blockheader32_v2 bhdr;

	if(len < sizeof(bhdr)) {
		return XLATE_TB_STATUS_INVALID_TABLE;
	}

	memcpy(&bhdr,curbase, sizeof(bhdr));
	if(bhdr.bh_first_new_addr) {
		bhdr.bh_first_new_addr -= movement;
	}
	if(bhdr.bh_first_old_addr) {
		bhdr.bh_first_old_addr -= movement;
	}
	if(bhdr.bh_low_old_addr) {
		bhdr.bh_low_old_addr -= movement;
	}
	if(bhdr.bh_high_old_addr) {
		bhdr.bh_high_old_addr -= movement;
	}
	
	memcpy(curbase,&bhdr,sizeof(bhdr));
	return XLATE_TB_STATUS_NO_ERROR;
}

/*
	Translate the header of a 32 bit v2 xlate header
*/
static int do_xlate_fix_32(void *pxdata,
		unsigned int len,
		unsigned int movement)
{
	xlate_header32_v2 phdr;
	unsigned int i;
	char *curbase = (char *)pxdata;
	unsigned int lenleft = len;

	if(len < sizeof(phdr)){
		return XLATE_TB_STATUS_INVALID_TABLE;
	}

	/* Update in local properly aligned struct
	   then copy back.
	*/
	memcpy(&phdr,pxdata, sizeof(phdr));

	if(phdr.hd_new_addr_low) {
	   phdr.hd_new_addr_low -= movement;
	}
	if(phdr.hd_new_addr_high) {
	   phdr.hd_new_addr_high -= movement;
	}
	if(phdr.hd_old_addr_low) {
	   phdr.hd_old_addr_low -= movement;
	}
	if(phdr.hd_old_addr_high) {
	   phdr.hd_old_addr_high -= movement;
	}

	if(phdr.hd_startup_fwa) {
	   phdr.hd_startup_fwa -= movement;
	}
	if(phdr.hd_startup_lwa) {
	   phdr.hd_startup_lwa -= movement;
	}
	memcpy(pxdata,&phdr,sizeof(phdr));

	curbase = curbase + sizeof(phdr);
	lenleft -= sizeof(phdr);
	for(i = 0; i <phdr.hd_num_blocks;
		 ++i, 
		curbase += sizeof(xlate_blockheader32_v2),
		lenleft -= sizeof(xlate_blockheader32_v2)) {

		int res;

		res = update_blockheader32(curbase,lenleft,movement);
		if(res != XLATE_TB_STATUS_NO_ERROR) {
			return res;
		}
	}

	curbase += (phdr.hd_num_blocks *phdr.hd_block_size);

	if( phdr.hd_reg_info_size > 0 ) {
        	char *reginfo = curbase;
        	char *reginfo_end  = curbase + phdr.hd_reg_info_size;
		int is64bit = 0;
		int table_version = (int)phdr.hd_version;
		int res;
		
                int want_instrs = 0; /* do want instr, not pc rel stuff */
        	Elf64_Addr       inputpc = 0;
        	Dwarf_Regtable  regtable;
        	Elf64_Xword     num_instrs;
        	Elf64_Xword     thisinst;
        	xlate_reg_instr2 * instrs_out;
	     	if(reginfo_end > ((char *)pxdata) + len) {
			return XLATE_TB_STATUS_INVALID_TABLE;
	     	}
	     	res = _xlate_expand_reg_info_internal_given_ptrs(
		         reginfo,reginfo_end,is64bit,table_version,
		         want_instrs,inputpc,
			&regtable,&num_instrs,&instrs_out);
             	if(res != XLATE_TB_STATUS_NO_ERROR) {
		  return res;
	     	}
		for(thisinst = 0; thisinst <num_instrs; ++thisinst) {
			char *instoff = 
				instrs_out[thisinst].sr_instr_offset +
				reginfo;
			Elf32_Word addr32;
			Elf64_Xword addr64;
			

			if(instrs_out[thisinst].sr_op == DW_CFA_set_loc) {
			  ++instoff; /* points at loc now */
			  if(is64bit) {
			    memcpy(&addr64,instoff,sizeof(addr64));
			    addr64 -= movement;
			    memcpy(instoff,&addr64,sizeof(addr64));
			  } else {
			    memcpy(&addr32,instoff,sizeof(addr32));
			    addr32 -= movement;
			    memcpy(instoff,&addr32,sizeof(addr32));
			  }
			}
		}

	}
	return XLATE_TB_STATUS_NO_ERROR;
	
}

/*
   We do copies so the caller need not have properly aligned data.
*/
static int
update_blockheader64(char *curbase,unsigned long long len,
		unsigned long long movement)
{
        xlate_blockheader64_v2 bhdr;

        if(len < sizeof(bhdr)) {
                return XLATE_TB_STATUS_INVALID_TABLE;
        }

        memcpy(&bhdr,curbase, sizeof(bhdr));
        if(bhdr.bh_first_new_addr) {
                bhdr.bh_first_new_addr -= movement;
        }
        if(bhdr.bh_first_old_addr) {
                bhdr.bh_first_old_addr -= movement;
        }
        if(bhdr.bh_low_old_addr) {
                bhdr.bh_low_old_addr -= movement;
        }
        if(bhdr.bh_high_old_addr) {
                bhdr.bh_high_old_addr -= movement;
        }
      
        memcpy(curbase,&bhdr,sizeof(bhdr));
        return XLATE_TB_STATUS_NO_ERROR;
}

/*
	Translate the header of a 64 bit v2 xlate header
*/
static int do_xlate_fix_64(void *pxdata,unsigned long long len,
		unsigned long long movement)
{
	xlate_header64_v2 phdr;
	unsigned long long i;
	char *curbase = (char *)pxdata;
	unsigned long long  lenleft = len;

	if(len < sizeof(phdr)){
		return XLATE_TB_STATUS_INVALID_TABLE;
	}

	/* Update in local properly aligned struct
	   then copy back.
	*/
	memcpy(&phdr,pxdata, sizeof(phdr));
	if(phdr.hd_new_addr_low) {
	   phdr.hd_new_addr_low -= movement;
	}
	if(phdr.hd_new_addr_high) {
	   phdr.hd_new_addr_high -= movement;
	}
	if(phdr.hd_old_addr_low) {
	   phdr.hd_old_addr_low -= movement;
	}
	if(phdr.hd_old_addr_high) {
	   phdr.hd_old_addr_high -= movement;
	}

	if(phdr.hd_startup_fwa) {
	   phdr.hd_startup_fwa -= movement;
	}
	if(phdr.hd_startup_lwa) {
	   phdr.hd_startup_lwa -= movement;
	}
	memcpy(pxdata,&phdr,sizeof(phdr));

	curbase = curbase + sizeof(phdr);
	lenleft -= sizeof(phdr);
	for(i = 0; i <phdr.hd_num_blocks;
		 ++i, 
		curbase += sizeof(xlate_blockheader64_v2),
		lenleft -= sizeof(xlate_blockheader64_v2)) {

		int res;

		res = update_blockheader64(curbase,lenleft,movement);
		if(res != XLATE_TB_STATUS_NO_ERROR) {
			return res;
		}
	}

	curbase += (phdr.hd_num_blocks *phdr.hd_block_size);

	if( phdr.hd_reg_info_size > 0 ) {
        	char *reginfo = curbase;
        	char *reginfo_end  = curbase + phdr.hd_reg_info_size;
		int is64bit = 0;
		int table_version = (int)phdr.hd_version;
		
                int want_instrs = 0; /* do want instr, not pc rel stuff */
		int res;
        	Elf64_Addr       inputpc = 0;
        	Dwarf_Regtable  regtable;
        	Elf64_Xword     num_instrs;
        	Elf64_Xword     thisinst;
        	xlate_reg_instr2 * instrs_out;
	     	if(reginfo_end > ((char *)pxdata) + len) {
			return XLATE_TB_STATUS_INVALID_TABLE;
	     	}
	     	res = _xlate_expand_reg_info_internal_given_ptrs(
		         reginfo,reginfo_end,is64bit,table_version,
		         want_instrs,inputpc,
			&regtable,&num_instrs,&instrs_out);
             	if(res != XLATE_TB_STATUS_NO_ERROR) {
		  return res;
	     	}
		for(thisinst = 0; thisinst <num_instrs; ++thisinst) {
			char *instoff = 
				instrs_out[thisinst].sr_instr_offset +
				reginfo;
			Elf32_Word addr32;
			Elf64_Xword addr64;
			

			if(instrs_out[thisinst].sr_op == DW_CFA_set_loc) {
			  ++instoff; /* points at loc now */
			  if(is64bit) {
			    memcpy(&addr64,instoff,sizeof(addr64));
			    addr64 -= movement;
			    memcpy(instoff,&addr64,sizeof(addr64));
			  } else {
			    memcpy(&addr32,instoff,sizeof(addr32));
			    addr32 -= movement;
			    memcpy(instoff,&addr32,sizeof(addr32));
			  }
			}
		}

	}

	return XLATE_TB_STATUS_NO_ERROR;

}

/*
    This is the routine that is externally visible.
    We do not require the caller to properly align
    the memory on a 64 or 32 bit boundary.
*/
int _xlate_fix_addresses_for_dso_movement(
        void*                pxlate,  /* memory address of the section:
			caller must read/mmap in  to memory and
			and pass pxlate as
			a pointer to the memory.
			*/
        unsigned long long len, /* Length of the xlate
				section.  If largerthan
			        the space pointed to by pxlate
				chaos will surely follow*/
        unsigned long long movement /* Amount that the 
				text moved. 
			This value will be added to the appropriate
			vm addresses in the section. */
		) 
{
	int res =  XLATE_TB_STATUS_NO_ERROR;

	xlate_header32_v2 phdr32;

	if(len == 0 || pxlate == 0) {
		return res;
	}

	if(len < sizeof(phdr32) ) {
		return XLATE_TB_STATUS_INVALID_TABLE;
	}
	memcpy(&phdr32,pxlate,sizeof(phdr32));
	switch(phdr32.hd_version) {
	case XLATE_TB_32_V2:
	    res = do_xlate_fix_32(pxlate,len,movement);
	    break;
	case XLATE_TB_64_V2:
	    res = do_xlate_fix_64(pxlate,len,movement);
	    break;
	case XLATE_TB_MAIN_V1:
	default:
	    /* Meaning invalid or
		 we are simply not handling this table 
		We never plan on handling the old forms here.
		No reason I know of to do so. davea 11/96
	    */
	    res = XLATE_TB_STATUS_XLATE_BAD;
	    break;
	}
	return res;
}

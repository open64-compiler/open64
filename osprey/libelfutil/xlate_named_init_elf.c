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

Given an Elf *, open up an xlate consumer handle

This one really does the work  of intializing.

xlate_named_init_elf.c

$Revision: 1.1.1.1 $
$Date: 2005/10/21 19:00:00 $

*/
#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_named_init_elf_xtnd  = _xlate_named_init_elf_xtnd
#pragma weak xlate_finish_xtnd  = _xlate_finish_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_named_init_elf  = _xlate_named_init_elf
#pragma weak xlate_finish  = _xlate_finish
#endif

#define streq(a,b) (strcmp(a,b) == 0)
static int
_xlate_initialize_v1_table(xlate_table_con tab,int is64bit)
{
  xlate_header_v1 *lclhdrp;
  char * end_addr;
  Uword len;

  if(tab->xc_data_size <  sizeof(xlate_header_v1)) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  tab->xc_block_headers = 
	(char *)tab->xc_section_data + sizeof(xlate_header_v1);
  lclhdrp = (xlate_header_v1*)tab->xc_section_data;

  tab->xc_hdr.ich_version = (int)lclhdrp->hd_version;
  tab->xc_hdr.ich_tablekind = (unsigned char)lclhdrp->hd_tablekind;
  if(is64bit  != lclhdrp->hd_is64_bit) {
	return XLATE_TB_STATUS_INCONSISTENT_64_BIT_INFO;
  }
  tab->xc_hdr.ich_block_size          = lclhdrp->hd_block_size;
  tab->xc_hdr.ich_num_blocks          = lclhdrp->hd_num_blocks;
  tab->xc_hdr.ich_total_num_entries   = lclhdrp->hd_num_entries;
  tab->xc_hdr.ich_total_reginfo_bytes = lclhdrp->hd_reg_info_size;
  tab->xc_hdr.ich_new_addr_low        = lclhdrp->hd_new_addr_low;
  tab->xc_hdr.ich_new_addr_high       = lclhdrp->hd_new_addr_high;
  tab->xc_hdr.ich_old_addr_low        = lclhdrp->hd_old_addr_low;
  tab->xc_hdr.ich_old_addr_high       = lclhdrp->hd_old_addr_high;
  tab->xc_ubh_array = calloc(sizeof(uniform_block_hdr),
		tab->xc_hdr.ich_num_blocks);
  if(tab->xc_ubh_array == 0) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
  }
  if(tab->xc_is64bit) {
	tab->xc_hdr.ich_new_addr_low  |= 
		((Elf64_Addr)lclhdrp->hd_upper32_bits_new) <<32;
	tab->xc_hdr.ich_new_addr_high |= 
		((Elf64_Addr)lclhdrp->hd_upper32_bits_new) <<32;
	tab->xc_hdr.ich_old_addr_low  |=
		((Elf64_Addr)lclhdrp->hd_upper32_bits_old) <<32;
	tab->xc_hdr.ich_old_addr_high |= 
		((Elf64_Addr)lclhdrp->hd_upper32_bits_old) <<32;
  }
  tab->xc_leb_data_blocks = tab->xc_block_headers +
	(Uword)(tab->xc_hdr.ich_num_blocks*sizeof(xlate_blockheader_v1));
  end_addr = tab->xc_leb_data_blocks +  
	(Uword)(tab->xc_hdr.ich_num_blocks*tab->xc_hdr.ich_block_size);
  tab->xc_reginfo_data = end_addr;

  end_addr += tab->xc_hdr.ich_total_reginfo_bytes;

  len = end_addr - tab->xc_section_data;
  if(len > tab->xc_data_size) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  
  tab->xc_get_info_func                = _xlate_get_infov1;
  tab->xc_search_for_addr_new          = _xlate_binary_search_for_addr;
  switch(tab->xc_hdr.ich_tablekind) {
  case xlate_tk_general:
      tab->xc_search_for_addr_old      = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block     = _xlate_get_range_from_block_v1_ge;
      break;
  case xlate_tk_preserve_size:
      tab->xc_search_for_addr_old      = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block     = _xlate_get_range_from_block_v1_ps;
      break;
  case xlate_tk_preserve_order:
      tab->xc_search_for_addr_old      = _xlate_binary_search_for_addr;

      tab->xc_get_range_from_block     = _xlate_get_range_from_block_v1_po;
      
      break;
  default:
      return XLATE_TB_STATUS_BAD_TABLEKIND;
  }
  tab->xc_block_header_indexer         = _xlate_index_into_blockhdrs_v1;
 
  return XLATE_TB_STATUS_NO_ERROR;
}
static int
_xlate_initialize_32_v2_table(xlate_table_con tab)
{

  char * end_addr;
  Uword len;

  xlate_header32_v2* lclhdrp;

  if(tab->xc_data_size <  sizeof(xlate_header32_v2)) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  lclhdrp = (xlate_header32_v2*)tab->xc_section_data;
  tab->xc_block_headers =  tab->xc_section_data + sizeof(xlate_header32_v2);

  tab->xc_hdr.ich_version             = (int)lclhdrp->hd_version;
  tab->xc_hdr.ich_tablekind           = lclhdrp->hd_tablekind;
  tab->xc_hdr.ich_block_size          = lclhdrp->hd_block_size;
  tab->xc_hdr.ich_num_blocks          = lclhdrp->hd_num_blocks;
  tab->xc_hdr.ich_total_num_entries   = lclhdrp->hd_num_entries;
  tab->xc_hdr.ich_total_reginfo_bytes = lclhdrp->hd_reg_info_size;
  tab->xc_hdr.ich_new_addr_low        = lclhdrp->hd_new_addr_low;
  tab->xc_hdr.ich_new_addr_high       = lclhdrp->hd_new_addr_high;
  tab->xc_hdr.ich_old_addr_low        = lclhdrp->hd_old_addr_low;
  tab->xc_hdr.ich_old_addr_high       = lclhdrp->hd_old_addr_high;
  tab->xc_leb_data_blocks             = tab->xc_block_headers +
	(Uword)(tab->xc_hdr.ich_num_blocks*sizeof(xlate_blockheader32_v2));
  end_addr = tab->xc_leb_data_blocks +  
	(Uword)(tab->xc_hdr.ich_num_blocks*tab->xc_hdr.ich_block_size);
  tab->xc_reginfo_data 		      = end_addr;
  tab->xc_ubh_array = calloc(sizeof(uniform_block_hdr),
                tab->xc_hdr.ich_num_blocks);
  if(tab->xc_ubh_array == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }


  end_addr += tab->xc_hdr.ich_total_reginfo_bytes;

  len = end_addr - tab->xc_section_data;
  if(len > tab->xc_data_size) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  
  tab->xc_get_info_func = _xlate_get_infov2_32;

  tab->xc_search_for_addr_new      =  _xlate_binary_search_for_addr;
  switch(tab->xc_hdr.ich_tablekind) {
  case xlate_tk_general:
      tab->xc_search_for_addr_old  = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_32_ge;
      break;    
  case xlate_tk_preserve_size:
      tab->xc_search_for_addr_old  = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_32_ps;
      break;
  case xlate_tk_preserve_order:
      tab->xc_search_for_addr_old  = _xlate_binary_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_32_po;
      break;
  default:
      return XLATE_TB_STATUS_BAD_TABLEKIND;
  }
  tab->xc_block_header_indexer     = _xlate_index_into_blockhdrs_v2_32;

  
  return XLATE_TB_STATUS_NO_ERROR;
}
static int
_xlate_initialize_64_v2_table(xlate_table_con tab)
{
  xlate_header64_v2 *lclhdrp;
  char * end_addr;
  Uword len;

  if(tab->xc_data_size <  sizeof(xlate_header64_v2)) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  tab->xc_block_headers = (char *)(tab->xc_section_data) + 
		sizeof(xlate_header64_v2);
  lclhdrp = (xlate_header64_v2*)tab->xc_section_data;

  tab->xc_hdr.ich_version             = (int)lclhdrp->hd_version;
  tab->xc_hdr.ich_tablekind           = lclhdrp->hd_tablekind;
  tab->xc_hdr.ich_block_size          = lclhdrp->hd_block_size;
  tab->xc_hdr.ich_num_blocks          = lclhdrp->hd_num_blocks;
  tab->xc_hdr.ich_total_num_entries   = lclhdrp->hd_num_entries;
  tab->xc_hdr.ich_total_reginfo_bytes = lclhdrp->hd_reg_info_size;
  tab->xc_hdr.ich_new_addr_low        = lclhdrp->hd_new_addr_low;
  tab->xc_hdr.ich_new_addr_high       = lclhdrp->hd_new_addr_high;
  tab->xc_hdr.ich_old_addr_low        = lclhdrp->hd_old_addr_low;
  tab->xc_hdr.ich_old_addr_high       = lclhdrp->hd_old_addr_high;
  tab->xc_leb_data_blocks             = tab->xc_block_headers +
	(Uword)(tab->xc_hdr.ich_num_blocks*sizeof(xlate_blockheader64_v2));
  end_addr                            = tab->xc_leb_data_blocks +  
	(Uword)(tab->xc_hdr.ich_num_blocks*tab->xc_hdr.ich_block_size);
  tab->xc_reginfo_data                = end_addr;

  tab->xc_ubh_array = calloc(sizeof(uniform_block_hdr),
                tab->xc_hdr.ich_num_blocks);
  if(tab->xc_ubh_array == 0) {
        return XLATE_TB_STATUS_ALLOC_FAIL;
  }

  end_addr += tab->xc_hdr.ich_total_reginfo_bytes;

  len = end_addr - tab->xc_section_data;
  if(len > tab->xc_data_size) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  
  tab->xc_get_info_func            = _xlate_get_infov2_64;
  tab->xc_search_for_addr_new      =  _xlate_binary_search_for_addr;
  switch(tab->xc_hdr.ich_tablekind) {
  case xlate_tk_general:
      tab->xc_search_for_addr_old  = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_64_ge;
      break;
  case xlate_tk_preserve_size:
      tab->xc_search_for_addr_old  = _xlate_special_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_64_ps;
      break;
  case xlate_tk_preserve_order:
      tab->xc_search_for_addr_old  = _xlate_binary_search_for_addr;
      tab->xc_get_range_from_block = _xlate_get_range_from_block_v2_64_po;
      break;
  default:
      return XLATE_TB_STATUS_BAD_TABLEKIND;
  }
  tab->xc_block_header_indexer     = _xlate_index_into_blockhdrs_v2_64;

  
  return XLATE_TB_STATUS_NO_ERROR;
}



/*
We have a pointer to the memory area. Here we determine that
table type and finish filling out the table structure.

The only way this can fail is if the table is truncated or
inconsistent.

The is64bit argument is the value from the elf file.
*/
static int
_xlate_fill_in_table_data(xlate_table_con tab,int is64bit)
{
  int retstatus = XLATE_TB_STATUS_NO_ERROR;

  xlate_header32_v2 tabhdr;

  if(tab->xc_data_size <  sizeof(Elf32_Word)) {
	return XLATE_TB_STATUS_SECTION_TOO_SMALL;
  }
  memcpy(&tabhdr,tab->xc_section_data,sizeof(Elf32_Word));
  
  tab->xc_is64bit = is64bit;

  switch(tabhdr.hd_version) {
  case XLATE_TB_COPY_V1:
  case XLATE_TB_DEBUG_V1:
  case XLATE_TB_OLD_V1 :
  default:
 	return XLATE_TB_STATUS_BAD_TABLEKIND;

  case  XLATE_TB_MAIN_V1:
	retstatus = _xlate_initialize_v1_table(tab,is64bit);
	break;
  case XLATE_TB_32_V2:
	if(is64bit) {
	   return XLATE_TB_STATUS_INCONSISTENT_64_BIT_INFO;
	}
	retstatus = _xlate_initialize_32_v2_table(tab);
	break;
  case XLATE_TB_64_V2:
	if(!is64bit) {
	   return XLATE_TB_STATUS_INCONSISTENT_64_BIT_INFO;
	}
	retstatus = _xlate_initialize_64_v2_table(tab);
	break;
  }

  


  return retstatus;
}



int
xlate_named_init_elf(Elf * elf, const char *section_name,
	xlate_table_con * ret_tab_ptr)
{
   int retstatus  = XLATE_TB_STATUS_NO_ERROR;
   xlate_table_con newtab = 0;
   Elf_Scn *scn = 0;
   int is64bit = 0;
   char *ident;
   size_t identsize;
   Elf_Scn *xlscn = 0;
   Elf64_Shdr *shdr64 = 0;
   Elf32_Shdr *shdr32 = 0;
   Elf32_Ehdr *ehdr32 = 0;
   Elf64_Ehdr *ehdr64 = 0;
   Elf_Data   *xlatedata = 0;
   int stringindex = 0;
   char * sec_name;

   if(elf_kind(elf) != ELF_K_ELF) {
	return XLATE_TB_STATUS_NOT_ELF;
   }
   ident = elf_getident(elf,&identsize);

   if(ident == 0 || identsize < EI_NIDENT) {
     return  XLATE_TB_STATUS_ELF_IDENT_BAD;
   }
   if(ident[EI_CLASS] ==  ELFCLASS64) {
	is64bit = 1;
   }
   if(is64bit) {
	ehdr64 = elf64_getehdr(elf);
	if(ehdr64 == 0 ) {
	  return XLATE_TB_STATUS_ELF_EHDR_BAD;
	}
	stringindex = ehdr64->e_shstrndx;
   } else {
	ehdr32 = elf32_getehdr(elf);
	if(ehdr32 == 0 ) {
	  return XLATE_TB_STATUS_ELF_EHDR_BAD;
	}
	stringindex = ehdr32->e_shstrndx;
   }

   for(scn = elf_nextscn(elf,scn);  
	scn != 0 && xlscn == 0
		; scn = elf_nextscn(elf,scn)) {
	if(is64bit) {
	  shdr64 = elf64_getshdr(scn);
	  if(shdr64 == 0) {
	    return XLATE_TB_STATUS_ELF_SHDR_BAD;
	  }
	  sec_name = elf_strptr(elf,stringindex,shdr64->sh_name);
	  if(sec_name == 0) {
	    return XLATE_TB_STATUS_ELF_STRPTR_BAD;
	  }
	  if(shdr64->sh_type    != SHT_MIPS_XLATE &&
		shdr64->sh_type != SHT_MIPS_XLATE_DEBUG &&
		shdr64->sh_type != SHT_MIPS_XLATE_OLD ) {
		continue;
	  }
	  if(!streq(section_name,sec_name)) {
		continue;
	  }
	  xlscn = scn;
	  break;
	} else {
	  shdr32 = elf32_getshdr(scn);
	  if(shdr32 == 0) {
	    return XLATE_TB_STATUS_ELF_SHDR_BAD;
	  }
	  sec_name = elf_strptr(elf,stringindex,shdr32->sh_name);
	  if(sec_name == 0) {
	    return XLATE_TB_STATUS_ELF_STRPTR_BAD;
	  }
	  if(shdr32->sh_type    != SHT_MIPS_XLATE  &&
		shdr32->sh_type != SHT_MIPS_XLATE_DEBUG &&
		shdr32->sh_type != SHT_MIPS_XLATE_OLD ) {
		continue;
	  }
	  if(!streq(section_name,sec_name)) {
		continue;
	  }
	  xlscn = scn;
	  break;
	}
   }
   if(xlscn == 0) {
	return XLATE_TB_STATUS_NO_XLATE;
   }

   xlatedata = elf_getdata(xlscn,NULL);
   if(xlatedata == NULL || xlatedata->d_size == 0) {
	return XLATE_TB_STATUS_NO_XLATE_DATA;
   }
   
   newtab = (xlate_table_con)malloc(sizeof(struct xlate_table_con_s));
   if(newtab == NULL) {
	return XLATE_TB_STATUS_ALLOC_FAIL;
   }
   BZERO(newtab,sizeof(struct xlate_table_con_s));


   newtab->xc_elf = elf;


   newtab->xc_section_data = xlatedata->d_buf;
   newtab->xc_data_size = xlatedata->d_size;
   if(newtab->xc_data_size != xlatedata->d_size) {
	/* we have a gigantic section and are compiled only 32 bit.
	** we simply cannot handle and I don't think the
        ** mmap would have worked anyway so we cannot really
	** get here.
	*/
     free(newtab);
     return XLATE_TB_STATUS_SECTION_TOO_BIG;
   }

   retstatus = _xlate_fill_in_table_data(newtab,is64bit);
   if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
     free(newtab);
     return retstatus;
   }

   newtab->xc_valid_table         = VALID_TABLE_MAGIC;
   *ret_tab_ptr = newtab;
   return retstatus;
}

int
xlate_finish(xlate_table_con tab)
{
   int retstatus  = XLATE_TB_STATUS_NO_ERROR;

   if(!tab) {
	return XLATE_TB_STATUS_NULL_TABLE;
   }
   if(tab->xc_valid_table != VALID_TABLE_MAGIC) {
	return XLATE_TB_STATUS_INVALID_TABLE;
   }

   if(tab->xc_ubh_array) {
	unsigned long l;
	uniform_block_hdr *ubhp = tab->xc_ubh_array;
	
	for(l = 0; l < tab->xc_hdr.ich_num_blocks; ++l,++ubhp) {
		if(ubhp->ub_expanded_entries) {
		  free(ubhp->ub_expanded_entries);
		  ubhp->ub_expanded_entries = 0;
		}
	}
	free(tab->xc_ubh_array);
	tab->xc_ubh_array = 0;
   }
   if(tab->xc_did_elf_begin ) {
	elf_end(tab->xc_elf);
   }
   BZERO(tab,sizeof(*tab));
   free(tab);
   return retstatus;
}

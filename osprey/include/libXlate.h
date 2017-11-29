/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef _LIBXLATE_H
#define _LIBXLATE_H
#ifdef __cplusplus
extern "C" {
#endif

/*
    libXlate.h

    $Revision: 1.4 $

    All external symbols  and macros begin with one of the following
	XLATE_
   	_XLATE_
	xlate_
	_xlate_

    Link in the functions with -lelfutil

*/

#include "libelf/libelf.h" /* sets _LIBELF_XTND_EXPANDED_DATA if appropriate */
#include "dwarf.h"
#include "libdwarf.h"

#ifndef _XLATE_TABLEKIND_DEF
#define _XLATE_TABLEKIND_DEF
typedef enum {	
		xlate_tk_general,
		xlate_tk_preserve_size,
		xlate_tk_preserve_order
} xlate_tablekind;
#endif

#ifdef _LIBELF_XTND_EXPANDED_DATA
#define xlate_init_fd                   _xlate_init_fd_xtnd
#define xlate_init_elf                  _xlate_init_elf_xtnd
#define xlate_named_init_fd             _xlate_named_init_fd_xtnd
#define xlate_named_init_elf            _xlate_named_init_elf_xtnd
#define xlate_get_info                  _xlate_get_info_xtnd
#define xlate_address                   _xlate_address_xtnd
#define xlate_get_reg_rule              _xlate_get_reg_rule_xtnd
#define xlate_get_all_reg_rules         _xlate_get_all_reg_rules_xtnd
#define xlate_expand_reg_info           _xlate_expand_reg_info_xtnd
#define xlate_expand_reg_info2          _xlate_expand_reg_info2_xtnd
#define xlate_finish                    _xlate_finish_xtnd

#define xlate_pro_init                  _xlate_pro_init_xtnd
#define xlate_pro_add_info              _xlate_pro_add_info_xtnd
#define xlate_pro_add_reg_info          _xlate_pro_add_reg_info_xtnd
#define xlate_pro_add_range             _xlate_pro_add_range_xtnd
#define xlate_pro_disk_header           _xlate_pro_disk_header_xtnd
#define xlate_pro_disk_next_block       _xlate_pro_disk_next_block_xtnd
#define xlate_pro_finish                _xlate_pro_finish_xtnd
#endif


/*
    We use different opaque structs as handles for the 
    Producer and Consumer.
*/
typedef struct xlate_table_con_s  *xlate_table_con;
typedef struct xlate_table_pro_s  *xlate_table_pro;

/*
    This struct is used to return pieces of the fully expanded
    table to the user.  It is used to return the ranges that
    correspond to a given contiguous range.

    Shared by all versions.
*/
typedef struct xlate_block_s {
    Elf64_Addr		xe_new_address;
    Elf64_Xword		xe_new_range;
    Elf64_Addr		xe_old_address;
    Elf64_Xword		xe_old_range;
} xlate_block;


/******************** Consumer Interface *************************/

#define XLATE_OPEN_STD_TABLE   0
#define XLATE_OPEN_DEBUG_TABLE 1

int xlate_init_fd(int	/*fd*/,
    int			/*open_debug_table*/,
    xlate_table_con *	/*returned_table_pointer*/);

int xlate_init_elf(Elf	* /*elf*/,
    int			  /*open_debug_table*/,
    xlate_table_con *	  /*table*/);

int xlate_named_init_fd(int /*fd*/,
    const char *	    /*section_name*/,
    xlate_table_con *	    /*returned_table_pointer*/);

int xlate_named_init_elf(Elf * /*elf*/,
    const char *	       /*section_name*/,
    xlate_table_con *	       /*table*/);

int xlate_get_info(xlate_table_con /*table*/,
    Elf64_Sxword *      /*dataMoved*/,
    Elf64_Addr *	/*new_low_addr*/,
    Elf64_Addr *	/*old_low_addr*/,
    Elf64_Addr *	/*new_high_addr*/,
    Elf64_Addr *	/*old_high_addr*/,
    Elf64_Addr *	/*startup_fwa*/,
    Elf64_Addr *	/*startup_lwa*/,
    Elf64_Xword *       /*number_of_ranges*/,
    int         *        /*old_text_exists*/,
    int         *	/*old_text_alloc*/,
    int		*       /*is64bit */,
    xlate_tablekind *   /*tablekind */,
    int		*       /*tableversion */);

#define XLATE_ADDRESS_INPUT_NEW_ADDRESS  1
#define XLATE_ADDRESS_INPUT_OLD_ADDRESS  0
int xlate_address(xlate_table_con /*table*/,
    int		   /*is_new_address*/,
    Elf64_Addr     /*address_in*/,
    Elf64_Addr *   /*address_out*/,
    xlate_block *  /*range*/);

int xlate_get_reg_rule(xlate_table_con  /*table*/,
    Elf64_Addr          /*pc*/,
    Elf32_Word          /*reg*/,
    Elf32_Word *	/*rule_register*/,
    Elf64_Xword *	/*rule_offset*/,
    Elf32_Word *	/*rule_is_offset*/);

int xlate_get_all_reg_rules(xlate_table_con  /*table*/,
    Elf64_Addr          /*pc*/,
    Dwarf_Regtable *    /*reg_table*/);

typedef struct xlate_reg_instr_s{
    Dwarf_Small		sr_op;
    Dwarf_Unsigned	sr_val1;
    Dwarf_Unsigned	sr_val2;
} xlate_reg_instr;

/* xlate_expand_reg_info is obsolescent call: 
** use xlate_expand_reg_info2 instead.
*/
int xlate_expand_reg_info(xlate_table_con  /*table*/,
    Elf64_Xword *	/*num_instrs*/,
    xlate_reg_instr **	/*instructions*/);

/*  xlate_reg_instr2_s is identical to
**  xlate_reg_instr_s except it has a new field and
**  uses a new interface function
**  to preserve binary compatibility for old xlate_expand_reg_info 
**  calling  code.
**  The sr_instr_offset is the byte offset in the register instructions
**  of the register instruction involved.
*/
typedef struct xlate_reg_instr2_s{
    Dwarf_Small		sr_op;
    Dwarf_Unsigned	sr_val1;
    Dwarf_Unsigned	sr_val2;
    Dwarf_Unsigned      sr_instr_offset;
} xlate_reg_instr2;

int xlate_expand_reg_info2(xlate_table_con  /*table*/,
    Elf64_Xword *	/*num_instrs*/,
    xlate_reg_instr2 **	/*instructions*/);

int xlate_finish(xlate_table_con /*table*/);


/***************** special rqs interface *******************/

/*
   Interface to xlate section fixer.
   Called only by rqs.
   Updates the bytes pointed to by pxlate, which must
   be a valid .MIPS.xlate section (read in by the caller).
   This routine does no file io and writes no messages.

   Returns value indicating success/failure.

   Returns XLATE_TB_STATUS_NO_ERROR 
   in case of successful updating.

   Returns XLATE_TB_STATUS_INVALID_TABLE
	if the table passed in is improper (perhaps partly trashed).

   Returns XLATE_TB_STATUS_XLATE_BAD  if the table has an improper
	version number.

   Updating can only fail if the section is illogical/incomplete
   (corruputed in some way).

*/
int _xlate_fix_addresses_for_dso_movement(
        void*                pxlate,  /* memory address of the section:
                        The caller must read or map in the section
                        and pass pxlate as
                        a pointer to the memory.
			No particular alignment of the section
			in memory is required by this routine.
                        */
        unsigned long long len, /* Length of the xlate
                                section.  If largerthan
                                the space pointed to by pxlate
                                chaos will surely follow*/
        unsigned long long movement /* Amount that the 
                                text moved. 
                        This value will be added to the appropriate
                        vm addresses in the section. */
                );

/***************** Producer Interface *******************/

int xlate_pro_init(xlate_table_pro * 	/*ret_table*/,
    xlate_tablekind     		/*tablekind*/,
    xlate_table_con 			/*compose_with_table*/,
    int         			/*is64Bit*/);

int xlate_pro_add_info(xlate_table_pro /*table*/,
    Elf64_Sxword         /*data_moved*/,
    Elf64_Addr          /*startup_fwa*/,
    Elf64_Addr          /*startup_lwa*/,
    Elf32_Word          /*old_text_exists*/,
    Elf32_Word          /*old_text_alloc*/);

int xlate_pro_add_reg_info(xlate_table_pro /*table*/,
    Dwarf_Small         /*op*/,
    Dwarf_Unsigned      /*val1*/,
    Dwarf_Unsigned      /*val2*/);

int xlate_pro_add_range(xlate_table_pro /*table*/,
    Elf64_Addr        	/*new_address*/,
    Elf64_Xword		/*new_range*/,
    Elf64_Addr        	/*old_address*/,
    Elf64_Xword        	/*old_range*/);

/* these are the legal values passed to xlate_pro_disk_header */
#define XLATE_PRO_STANDARD_SETUP  1
#define XLATE_PRO_DEBUG_SETUP     2

int xlate_pro_disk_header(xlate_table_pro  /*table*/,
    int		/*standard_or_debug*/,
    Elf64_Xword *	/*total_memory_req*/,
    Elf64_Xword *	/*num_blocks*/);

int xlate_pro_disk_next_block(xlate_table_pro /*table*/,
    char **		/*data*/,
    Elf64_Xword *	/*data_size*/);


int xlate_pro_finish(xlate_table_pro /*table*/);


/***************** Return values *******************/


/*
  0 means no error.
  All errors are negative numbers.  Defined here.
  Positive number never returned.
*/
#define XLATE_TB_STATUS_NO_ERROR		0
#define XLATE_TB_STATUS_ALLOC_FAIL		-1
#define XLATE_TB_STATUS_NULL_TABLE		-2
#define XLATE_TB_STATUS_BAD_TABLEKIND		-3
#define XLATE_TB_STATUS_BAD_ADD_ADDR		-4
#define XLATE_TB_STATUS_RET_ADDR_NULL		-6
#define XLATE_TB_STATUS_NO_MORE_BLOCKS		-7
#define XLATE_TB_STATUS_NOT_YET_IMPLEMENT	-8
#define XLATE_TB_STATUS_NO_DEBUG		-9
#define XLATE_TB_STATUS_ALREADY_DONE		-10

#define XLATE_TB_STATUS_ADDR_UNALIGNED		-11
#define XLATE_TB_STATUS_RANGE_BAD		-12
#define XLATE_TB_STATUS_FSTAT_ERROR		-13
#define XLATE_TB_STATUS_MMAP_ERROR		-14
#define XLATE_TB_STATUS_BAD_VERSION		-15
#define XLATE_TB_STATUS_NULL_HEADER		-16
#define XLATE_TB_STATUS_NO_HEADER		-17
#define XLATE_TB_STATUS_BAD_FILE_SIZE		-18
#define XLATE_TB_STATUS_NEW_ADDR_ERROR          -19 /* unused */
#define XLATE_TB_STATUS_DECODE_ERROR		-20

#define XLATE_TB_STATUS_BAD_BLOCK_INDEX		-21
#define XLATE_TB_STATUS_UPPER_ADDR_BAD		-22
#define XLATE_TB_STATUS_TABLE_NOT_PO		-23
#define XLATE_TB_STATUS_MUNMAP_ERROR		-24
#define XLATE_TB_STATUS_ELF_IDENT_BAD		-25
#define XLATE_TB_STATUS_ELF_SHDR_BAD		-26
#define XLATE_TB_STATUS_NO_XLATE		-27
#define XLATE_TB_STATUS_NO_XLATE_DATA		-28
#define XLATE_TB_STATUS_XLATE_BAD		-29
#define XLATE_TB_STATUS_XLATE_DEBUG_BAD		-30

#define XLATE_TB_STATUS_ELF_VERSION_BAD		-31
#define XLATE_TB_STATUS_ELF_BEGIN_BAD		-32
#define XLATE_TB_STATUS_NOT_ELF			-33
#define XLATE_TB_STATUS_OLD_ADDR_ERROR          -34 /* unused */
#define XLATE_TB_STATUS_ADD_TOO_LATE		-35
#define XLATE_TB_STATUS_BAD_REG_VAL		-36
#define XLATE_TB_STATUS_BAD_REG_OP		-37
#define XLATE_TB_STATUS_BAD_FRAME_OP		-38
#define XLATE_TB_STATUS_NO_REG_INFO		-39

#define XLATE_TB_STATUS_SECTION_TOO_BIG         -42
#define XLATE_TB_STATUS_INVALID_TABLE           -43
#define XLATE_TB_STATUS_SECTION_TOO_SMALL       -44
#define XLATE_TB_STATUS_INCONSISTENT_64_BIT_INFO -45
#define XLATE_TB_STATUS_NO_SUCH_ADDR_IN_TABLE   -46
#define XLATE_TB_STATUS_INVALID_BLOCK_INDEX     -47
#define XLATE_TB_STATUS_REG_REQUEST_BOGUS     	-48
#define XLATE_TB_STATUS_PRO_CON_TABLE_MISMATCH 	-59
#define XLATE_TB_STATUS_PRO_REQ_INVALID 	-50
#define XLATE_TB_STATUS_BLOCK_REQ_SEQ_ERR 	-51
#define XLATE_TB_STATUS_FRAME_RESTORE_INVALID 	-52
#define XLATE_TB_STATUS_UNEQUAL_RANGE 		-53
#define XLATE_TB_STATUS_INVALID_PO_INPUT 	-54
#define XLATE_TB_STATUS_INVALID_SEQUENCE 	-55
#define XLATE_TB_STATUS_ELF_EHDR_BAD            -56
#define XLATE_TB_STATUS_ELF_STRPTR_BAD          -57


#ifdef __cplusplus
}
#endif
#endif /* _LIBXLATE_H */

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


#ifndef _CMPLRS_XLATE_H
#define _CMPLRS_XLATE_H
/*
    xlate.h

    $Revision$

    Address translation table.

    Version 1 refers to the tables created by IRIX6.2 and MIPSpro7
	pixie and cord.

    Version 2 refers to the tables created by later versions.

    For type definitions, see elf.h, dwarf.h, libdwarf.h

    For a functional definition see <libXlate.h>

*/
#ifdef __cplusplus
extern "C" {
#endif

#ifndef _XLATE_TABLEKIND_DEF
#define _XLATE_TABLEKIND_DEF
typedef enum xlate_tablekind_e {
                xlate_tk_general,
                xlate_tk_preserve_size,
                xlate_tk_preserve_order
} xlate_tablekind;
#endif

/* hd_version: version 1 magic numbers */
#define XLATE_TB_MAIN_V1                0x1
#define XLATE_TB_COPY_V1                0x2
#define XLATE_TB_DEBUG_V1               0x3 /* unused */
#define XLATE_TB_OLD_V1                 0x4 /* unused */

/* hd_version: version 2 magic numbers */
#define XLATE_TB_32_V2                     0x5 /* 32 bit table */
#define XLATE_TB_64_V2                     0x6 /* 64 bit table */


/* 
  Version 1 header struct actually written on 
  disk for each data block. 
  Same format used for 32 and 64 bit applications.
*/
typedef struct xlate_blockheader_v1_s {
    Elf32_Word		bh_first_new_addr;
    Elf32_Word		bh_first_old_addr;
    Elf32_Word		bh_num_entries;
} xlate_blockheader_v1;

/*
  Version  1 section header for the actual table. 
  Written on disk. Both 64 and 32 bit applications.
*/
typedef struct xlate_header_v1_s {

    /* Version number/ magic number.  */
    Elf32_Word		hd_version; /* 1,2,3, or 4 */

	/* what kind of table this header represents.  */
    Elf32_Word		hd_tablekind;

	/* Flag indicating whether object is 64-bits. */
    Elf32_Word		hd_is64_bit;

	/* Upper word of all new addresses being added. 
		(zero in a 32-bit-pointer app)*/
    Elf32_Word		hd_upper32_bits_new;

	/* Upper word of all old addresses being added. 
		(zero in a 32-bit-pointer app)*/
    Elf32_Word		hd_upper32_bits_old;

	/* Size of each data block in the table. */
    Elf32_Sword		hd_block_size;

	/* Number of data blocks in this table. */
    Elf32_Sword		hd_num_blocks;

	/* Number of range entries in this table. */
    Elf32_Sword		hd_num_entries;

	/* Lowest and highest of all new addresses in this table. */
    Elf32_Word		hd_new_addr_low;
    Elf32_Word		hd_new_addr_high;

	/* Lowest and highest of all old addresses in this table. */
    Elf32_Word		hd_old_addr_low;
    Elf32_Word		hd_old_addr_high;

	/* Number of bytes data was moved. */
    Elf32_Sword		hd_data_moved;

	/* 
	    Address of First and Last instruction of 
	    pixie (for example) startup code.  
	    0 if no startup code present.
	*/
    Elf32_Word		hd_startup_fwa;
    Elf32_Word		hd_startup_lwa;


	/* Old Text is saved in object.  value: 0 or 1*/
    Elf32_Word		hd_old_text_exists;

	/* Saved old text is in a loadable segment. value: 0 or 1 */
    Elf32_Word		hd_old_text_alloc;

	/* Size of bytes in the Register Info area. */
    Elf32_Word		hd_reg_info_size;
} xlate_header_v1;

/******* version 2 format for 32 bit pointer apps **********/

typedef struct xlate_blockheader32_v2_s {
    Elf32_Word		bh_first_new_addr;
    Elf32_Word		bh_first_old_addr;
    Elf32_Word		bh_num_entries;
    Elf32_Word		bh_low_old_addr;
    Elf32_Word		bh_high_old_addr;
} xlate_blockheader32_v2;


/* data is arranged smallest to largest after version number. */
typedef struct xlate_header32_v2_s {

    /* Version number/ magic number.  */
    Elf32_Word		hd_version; /* == 5 */

	/* what kind of table this header represents.  */
    unsigned char	hd_tablekind;

	/* Old Text is saved in object.  value: 0 or 1*/
    unsigned char	hd_old_text_exists;

	/* Saved old text is in a loadable segment. value: 0 or 1 */
    unsigned char	hd_old_text_alloc;

	/* Size of each data block in the table. */
    Elf32_Word		hd_block_size;

	/* Size of bytes in the Register Info area. */
    Elf32_Word		hd_reg_info_size;

	/* Number of data blocks in this table. */
    Elf32_Word		hd_num_blocks;

	/* Number of range entries in this table. */
    Elf32_Sword		hd_num_entries;

	/* Lowest and highest of all new addresses in this table. */
    Elf32_Addr		hd_new_addr_low;
    Elf32_Addr		hd_new_addr_high;

	/* Lowest highest of all old addresses in this table. */
    Elf32_Addr		hd_old_addr_low;
    Elf32_Addr		hd_old_addr_high;

	/* Number of bytes data was moved. */
    Elf32_Sword		hd_data_moved;

	/* 
	    Address of First and Last instruction of 
	    pixie (for example) startup code.  
	    0 if no startup code present.
	*/
    Elf32_Addr		hd_startup_fwa;
    Elf32_Addr		hd_startup_lwa;
} xlate_header32_v2;

/******* version 2 format for 64 bit pointer apps **********/

typedef struct xlate_blockheader64_v2_s {
    Elf64_Addr		bh_first_new_addr;
    Elf64_Addr		bh_first_old_addr;
    Elf64_Xword	        bh_num_entries;
    Elf64_Addr		bh_low_old_addr;
    Elf64_Addr		bh_high_old_addr;
} xlate_blockheader64_v2;

/* data is arranged smallest to largest after version number. */
typedef struct xlate_header64_v2_s {

    /* Version number/ magic number.  */
    Elf32_Word		hd_version;  /* == 6 */

	/* what kind of table this header represents.  */
    unsigned char	hd_tablekind;

	/* Old Text is saved in object.  value: 0 or 1*/
    unsigned char	hd_old_text_exists;

	/* Saved old text is in a loadable segment. value: 0 or 1 */
    unsigned char	hd_old_text_alloc;

	/* Size of each data block in the table. */
    Elf32_Word		hd_block_size;

	/* Size of bytes in the Register Info area. */
    Elf32_Word		hd_reg_info_size;

	/* Number of data blocks in this table. */
    Elf64_Xword		hd_num_blocks;

	/* Number of range entries in this table. */
    Elf64_Xword		hd_num_entries;

	/* Lowest and highest of all new addresses in this table. */
    Elf64_Addr		hd_new_addr_low;
    Elf64_Addr		hd_new_addr_high;

	/* Lowest and highest of all old addresses in this table. */
    Elf64_Addr		hd_old_addr_low; 
    Elf64_Addr		hd_old_addr_high;

	/* Number of bytes data was moved. */
    Elf64_Sxword	hd_data_moved;

	/* 
	    Address of First and Last instruction of 
	    pixie (for example) startup code.  
	    0 if no startup code present.
	*/
    Elf64_Addr		hd_startup_fwa;
    Elf64_Addr		hd_startup_lwa;
} xlate_header64_v2;


#ifdef __cplusplus
}
#endif
#endif  /* _CMPLRS_XLATE_H */


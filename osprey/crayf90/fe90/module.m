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


/* USMID:  "\n@(#)5.0_pl/macros/module.m	5.5	09/21/99 15:54:23\n" */

/* When we no longer support release 1.0 on any platform, we can throw out */
/* version 1 and 2 stuff.  When we no longer support 2.0 on any platform   */
/* we can throw out version 3 stuff.                                       */

/* 3.0 is released at version 11 - Also 7.2 */
/* 7.2+ is released at version 13 */
/* 3.1 is released at version 14  */
/* 3.2 is released at version 15 -  Also 7.3 */
/* 3.3 is released at version 15 */

/* 3.4 and 7.4 should be released at version 16 or later */

# define MD_CURRENT_VERSION		16
# define MD_LAST_1_0_VERSION		3
# define MD_LAST_2_0_VERSION		7
# define MD_LAST_3_0_VERSION		14  /* 3.0 starts at 11 */
# define MD_LAST_4_0_VERSION		15  /* 4.0 starts at 15 */
# define MD_LAST_5_0_VERSION		16  /* 5.0 starts at 16 */

# define MD_PDT_HEADER_BYTE_SIZE	sizeof(pdt_tbl_hdr_type)
# define MD_TBL_BYTE_SIZE            	sizeof(mit_header_type)
# define MD_TBL_SIZE			MD_TBL_BYTE_SIZE/TARGET_BYTES_PER_WORD
# define MD_DIRECTORY_BYTE_SIZE        	sizeof(mit_descriptor_type)

# if defined(_HOST32)
#    define MD_AFTER_PDT		(&mit_header.wd[2])
# else
#    define MD_AFTER_PDT		(&mit_header.wd[1])
# endif

# define OLD_MD_ALTERNATE_ENTRY		mit_header.old_tbl.alternate_entry
# define OLD_MD_NAME_LEN		mit_header.old_tbl.name_length

# define MD_ALTERNATE_ENTRY		mit_header.tbl.alternate_entry
# define MD_CF77TYPES			mit_header.tbl.cf77types
# define MD_DALIGN			mit_header.tbl.dalign
# define MD_DEFAULT_INTEGER_TYPE	mit_header.tbl.default_integer
# define MD_DEFAULT32			mit_header.tbl.default32
# define MD_DEFAULT64			mit_header.tbl.default64
# define MD_ENABLE_DOUBLE_PRECISION	mit_header.tbl.dp_hit_me
# define MD_FLOAT64			mit_header.tbl.float64
# define MD_HAS_ERRORS			mit_header.tbl.mod_has_errors
# define MD_MODULE			mit_header.tbl.module
# define MD_NAME_LEN			mit_header.tbl.name_length
# define MD_NEW_CONST_TBL		mit_header.tbl.new_cn_tbl
# define MD_POINTER8			mit_header.tbl.pointer8
# define MD_TARGET			mit_header.tbl.target
# define MD_VERSION_NUM			mit_header.tbl.version_num

# define MD_PDT_HDR_TYPE		mit_header.tbl.hdr.hdr_type
# define MD_PDT_HDR_LEN			mit_header.tbl.hdr.hdr_len
# define MD_NAME_PTR			&mit_header.tbl.name_char[0]

# if defined(_HOST32)
#   define MD_NAME_LONG			(&mit_header.wd[4])
# else
#   define MD_NAME_LONG			(&mit_header.wd[2])
# endif

# define MD_TBL_TYPE(IDX)               mit_descriptor[IDX].fld.tbl_type
# define MD_NUM_ENTRIES(IDX)            mit_descriptor[IDX].fld.num_entries


# define OUTPUT_TBL_TO_MODULE(FILE, MOD_ATTR_IDX, TBL)			       \
	 {int	_num_objs;						       \
	  if (TBL##_idx > NULL_IDX) {					       \
	     _num_objs	= fwrite(&TBL[1],				       \
				 sizeof(TBL##_type),			       \
				 TBL##_idx,				       \
				 FILE);					       \
	     if (_num_objs != TBL##_idx) {				       \
                PRINTMSG(AT_DEF_LINE(MOD_ATTR_IDX), 726, Error,		       \
			 AT_DEF_COLUMN(MOD_ATTR_IDX),			       \
			 AT_OBJ_NAME_PTR(MOD_ATTR_IDX));		       \
	     }								       \
	   }								       \
	 }

# define FSEEK(FILE_PTR, OFFSET, ORIGIN)				       \
	((fseek(FILE_PTR, OFFSET, ORIGIN) == 0) &&			       \
	 (ftell(FILE_PTR) <= mod_file_end_offset))

# define KEEP_ATTR(AT_IDX)						       \
	 if (!ML_AT_KEEP_ME(AT_IDX))	set_mod_link_tbl_for_attr(AT_IDX)

# define KEEP_CN(CN_IDX)						       \
	 if (!ML_CN_KEEP_ME(CN_IDX))	set_mod_link_tbl_for_cn(CN_IDX)

# define KEEP_IR(IR_IDX)						       \
	 if (!ML_IR_KEEP_ME(IR_IDX))	set_mod_link_tbl_for_ir(IR_IDX)

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


/* USMID:  "\n@(#)5.0_pl/headers/asm.h	5.1	04/29/99 21:22:31\n" */


/*
Include file of opcodes for cray intrinsic functions.
*/


/*
   For opcodes that require register numbers as such i or j, 
   they are set to 0 for that bit that contains the register 
   number except for intrinsic function "read_CA" in which 
   the j register is set to one, since j can not be 0.  
   A negative opcode indicates that there is no opcode 
   to represent the operation. 
*/




/* PVP Intrinsic Op Codes */
enum pvp_mif_asm_codes_tag {
	_getvm_op            = 0073000,
	_setvm_op            = 0003000,
	_cmr_op              = 0002700,
	_EX_op               = 0004000,
	_readSR_op           = 0073001,
	_readSB_op           = 0026007,
	_readST_op           = 0072003,
	_semget_op           = 0072002,
	_semput_op           = 0073002,
	_writeSB_op          = 0027006,
	_writeST_op          = 0073003,
	_semclr_op           = 0003600,
	_semset_op           = 0003700,
	_semts_op            = 0003400,
	_jts_op              = 0006400,
	_readSBi_op          = 0026006,
	_ESC_op              = 0002501,
	_DSC_op              = 0002601,
	_readbpc_op          = 0120060,
	_CCI_op              = 0001405,
	_CIPI_op             = 0001402,
	_clrCI_op            = 0001200,
	_DCI_op              = 0001407,
	_ECI_op              = 0001406,
	_ERR_op              = 0000000,
	_loadRT_op           = 0001400,
	_MC_op               = 0001201,
	_PCI_op              = 0001404,
	_readB_op            = 0024000,
	_readCI_op           = 0033000,
	_readCA_op           = 0033010,
	_readCE_op           = 0033001,
	_readXA_op           = 0001301,
	_readEA_op           = 0023006,
	_SETC_op             = 0001403,
	_setCA_op            = 0001000,
	_setEA_op            = 0027002,
	_setCL_op            = 0001100,
	_setXA_op            = 0001300,
	_SIPI_op             = 0001401,
	_DI_op               = 0001202,
	_DMI_op              = 0001303,
	_EMI_op              = 0001302,
	_EI_op               = 0001203,
	_ESI_op              = 0001600,
	_rjump_op            = 0007000,
	_writeBP_op          = 0001700,
	_writeSR_op          = 0073005,
	_chanDI_op           = -4,
	_chanEI_op           = -5,
	_int_mult_upper_op   = -6,
	_pvp_last_op    = 0777777
};

typedef enum pvp_mif_asm_codes_tag pvp_mif_asm_codes;


/* MPP Intrinsic Op Codes */

enum mpp_mif_asm_codes_tag {
	_memory_barrier_op                       = 2,
	_remote_write_barrier_op                 = 3,
	_write_memory_barrier_op                 = 5,
	_mpp_last_op                             = 99
};

typedef enum mpp_mif_asm_codes_tag mpp_mif_asm_codes;


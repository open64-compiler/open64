/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
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
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <isam.h>
#include "defs.h"
#include "strtab.h"
#include "stab.h"
#include "wn.h"
#include "wn_util.h"
#include "wio.h"
#include "config.h"
#include "targ_sim.h"
#include "targ_const.h"
#include "targ_const_private.h"
#include "const.h"
#include "flags.h"
#include "wn_lower.h"
#include "srcpos.h"
#include "stblock.h"
#include "be_symtab.h"
#include <isam.h>
#include <wn_fio.h>
#include "opt_points_to.h"
#include "fb_whirl.h"

#define STACK_LENGTH 25
#define DIM_OFFSET 56
#define DIM_SZ 24

#define GLOBAL_LEVEL 1
#define HOST_LEVEL GLOBAL_LEVEL + 1
#define INTERNAL_LEVEL HOST_LEVEL + 1


/* for use with the implied-do loop nesting type */
#define NO_NESTING	0
#define NESTED_DOPE	1	/* items inside an implied-do entry .  These
				** need to be turned into dope vector
				** when appropriate */
#define NESTED_ITEM	2	/* I/O list items inside a do loop generated
				** for an implied-do loop */

#define FIRST_CALL(x) (x & 2)
#define LAST_CALL(x)  (x & 1)
static OPCODE opc_lda;
static OPCODE opc_const;
static INT32  fcd_size;
static ST *stack_st = NULL;
static TY_IDX stack_ty = (TY_IDX) 0;
static ST *cilist_st = NULL;
static WN *cr_iostat1 = NULL;
static WN *cr_iostat2 = NULL;
static WN *copyout_block = NULL;
#define MAX_NESTED_IMPL	50
static ST *impl_idx[MAX_NESTED_IMPL];
static INT32 num_impl;
static INT32 first_last;


typedef struct impdo_info IMPDO_INFO;
struct impdo_info {
   ST *index;
   struct impdo_info *next;
};
struct marked_set {
   ST *st;
   struct marked_set *next;
};
typedef struct marked_set MARKED_SET;
static MARKED_SET *marked_set;

#define Impdo_index(x) ((x)->index)
#define Impdo_next(x) ((x)->next)
#define Marked_st(x) ((x)->st)
#define Marked_next(x) ((x)->next)
#define WNOPR(w) (WN_operator(w))
static BOOL cwh_io_analyse_io_item(WN *tree, IMPDO_INFO *impdo_set, INT32 mode);
static BOOL cwh_io_analyse_expr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode);
static BOOL cwh_io_analyse_arr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode);
static INT32 cwh_io_analyse_index_expr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode);
static void cwh_stab_free_auxst(void);
static INT32 member(ST *st, IMPDO_INFO *impdo_set);
static INT32 cwh_io_search_implied_do_index(WN *tree, IMPDO_INFO *impdo_set);
static WN * Substitute_1_For_Impdo_Index_Val(WN *tree, IMPDO_INFO *impdo);
static void cwh_io_unmark(void);
static void cwh_io_add_st_to_marked_set(ST *st);
static BOOL OPCODE_has_aux(const OPCODE opc);
static INT32 local_sequence; /* to distinguish different structs with same name */
static char seq_buff[10];
static IOLIB current_io_library;


#define MAX_DIM		7
static TY_IDX dope_vector_ty[ MAX_DIM+1 ];

typedef enum {

  FIOOPER_NONE = 0,
  FIOOPER_FIRST = 1,

    /* F77 external I/O operations. */

    FIO_EXT_READ_FORMAT_start   = 1,
    FIO_EXT_READ_UNFORMAT_start = 2,
    FIO_EXT_READ_LIST_start     = 3,
    FIO_EXT_READ_NAMELIST_start = 4,

    FIO_EXT_WRITE_FORMAT_start   = 5,
    FIO_EXT_WRITE_UNFORMAT_start = 6,
    FIO_EXT_WRITE_LIST_start     = 7,
    FIO_EXT_WRITE_NAMELIST_start = 8,

    FIO_EXT_REWRITE_FORMAT_start   = 9,
    FIO_EXT_REWRITE_UNFORMAT_start = 10,
    FIO_EXT_REWRITE_LIST_start     = 11,

    FIO_EXT_READ_FORMAT_end   = 12,
    FIO_EXT_READ_UNFORMAT_end = 13,
    FIO_EXT_READ_LIST_end     = 14,

    FIO_EXT_WRITE_FORMAT_end   = 15,
    FIO_EXT_WRITE_UNFORMAT_end = 16,
    FIO_EXT_WRITE_LIST_end     = 17,

    FIO_EXT_REWRITE_FORMAT_end   = 18,
    FIO_EXT_REWRITE_UNFORMAT_end = 19,
    FIO_EXT_REWRITE_LIST_end     = 20,

    /* F77 internal I/O operations. */

    FIO_INT_READ_FORMAT_start = 21,
    FIO_INT_READ_LIST_start   = 22,

    FIO_INT_WRITE_FORMAT_start = 23,
    FIO_INT_WRITE_LIST_start   = 24,

    FIO_INT_READ_FORMAT_end = 25,
    FIO_INT_READ_LIST_end   = 26,

    FIO_INT_WRITE_FORMAT_end = 27,
    FIO_INT_WRITE_LIST_end   = 28,

    /* F77 direct I/O operations. */

    FIO_DIR_READ_FORMAT_start   = 29,
    FIO_DIR_READ_UNFORMAT_start = 30,

    FIO_DIR_WRITE_FORMAT_start   = 31,
    FIO_DIR_WRITE_UNFORMAT_start = 32,

    FIO_DIR_READ_FORMAT_end   = 33,
    FIO_DIR_READ_UNFORMAT_end = 34,

    FIO_DIR_WRITE_FORMAT_end   = 35,
    FIO_DIR_WRITE_UNFORMAT_end = 36,

    /* F77 file transfer operations */

    FIO_FORMAT_ADDR4_item  = 37,
    FIO_FORMAT_ADDR8_item  = 38,
    FIO_FORMAT_CHAR_item   = 39,
    FIO_FORMAT_I1_item     = 40,
    FIO_FORMAT_I2_item     = 41,
    FIO_FORMAT_I4_item     = 42,
    FIO_FORMAT_I8_item     = 43,
    FIO_FORMAT_L1_item     = 44,
    FIO_FORMAT_L2_item     = 45,
    FIO_FORMAT_L4_item     = 46,
    FIO_FORMAT_L8_item     = 47,
    FIO_FORMAT_R4_item     = 48,
    FIO_FORMAT_R8_item     = 49,
    FIO_FORMAT_R16_item    = 50,
    FIO_FORMAT_C4_item     = 51,
    FIO_FORMAT_C8_item     = 52,
    FIO_FORMAT_C16_item    = 53,

    FIO_FORMAT_ADDR4_value = 54,
    FIO_FORMAT_ADDR8_value = 55,
    FIO_FORMAT_CHAR_value  = 56,
    FIO_FORMAT_I1_value    = 57,
    FIO_FORMAT_I2_value    = 58,
    FIO_FORMAT_I4_value    = 59,
    FIO_FORMAT_I8_value    = 60,
    FIO_FORMAT_L1_value    = 61,
    FIO_FORMAT_L2_value    = 62,
    FIO_FORMAT_L4_value    = 63,
    FIO_FORMAT_L8_value    = 64,
    FIO_FORMAT_R4_value    = 65,
    FIO_FORMAT_R8_value    = 66,
    FIO_FORMAT_R16_value   = 67,
    FIO_FORMAT_C4_value    = 68,
    FIO_FORMAT_C8_value    = 69,
    FIO_FORMAT_C16_value   = 70,

    FIO_UNFORMAT_ADDR4_item  = 71,
    FIO_UNFORMAT_ADDR8_item  = 72,
    FIO_UNFORMAT_CHAR_item   = 73,
    FIO_UNFORMAT_I1_item     = 74,
    FIO_UNFORMAT_I2_item     = 75,
    FIO_UNFORMAT_I4_item     = 76,
    FIO_UNFORMAT_I8_item     = 77,
    FIO_UNFORMAT_L1_item     = 78,
    FIO_UNFORMAT_L2_item     = 79,
    FIO_UNFORMAT_L4_item     = 80,
    FIO_UNFORMAT_L8_item     = 81,
    FIO_UNFORMAT_R4_item     = 82,
    FIO_UNFORMAT_R8_item     = 83,
    FIO_UNFORMAT_R16_item    = 84,
    FIO_UNFORMAT_C4_item     = 85,
    FIO_UNFORMAT_C8_item     = 86,
    FIO_UNFORMAT_C16_item    = 87,

    FIO_UNFORMAT_ADDR4_value = 88,
    FIO_UNFORMAT_ADDR8_value = 89,
    FIO_UNFORMAT_CHAR_value  = 90,
    FIO_UNFORMAT_I1_value    = 91,
    FIO_UNFORMAT_I2_value    = 92,
    FIO_UNFORMAT_I4_value    = 93,
    FIO_UNFORMAT_I8_value    = 94,
    FIO_UNFORMAT_L1_value    = 95,
    FIO_UNFORMAT_L2_value    = 96,
    FIO_UNFORMAT_L4_value    = 97,
    FIO_UNFORMAT_L8_value    = 98,
    FIO_UNFORMAT_R4_value    = 99,
    FIO_UNFORMAT_R8_value    = 100,
    FIO_UNFORMAT_R16_value   = 101,
    FIO_UNFORMAT_C4_value    = 102,
    FIO_UNFORMAT_C8_value    = 103,
    FIO_UNFORMAT_C16_value   = 104,

    FIO_LIST_ADDR4_item  = 105,
    FIO_LIST_ADDR8_item  = 106,
    FIO_LIST_CHAR_item   = 107,
    FIO_LIST_I1_item     = 108,
    FIO_LIST_I2_item     = 109,
    FIO_LIST_I4_item     = 110,
    FIO_LIST_I8_item     = 111,
    FIO_LIST_L1_item     = 112,
    FIO_LIST_L2_item     = 113,
    FIO_LIST_L4_item     = 114,
    FIO_LIST_L8_item     = 115,
    FIO_LIST_R4_item     = 116,
    FIO_LIST_R8_item     = 117,
    FIO_LIST_R16_item    = 118,
    FIO_LIST_C4_item     = 119,
    FIO_LIST_C8_item     = 120,
    FIO_LIST_C16_item    = 121,

    FIO_LIST_ADDR4_value = 122,
    FIO_LIST_ADDR8_value = 123,
    FIO_LIST_CHAR_value  = 124,
    FIO_LIST_I1_value    = 125,
    FIO_LIST_I2_value    = 126,
    FIO_LIST_I4_value    = 127,
    FIO_LIST_I8_value    = 128,
    FIO_LIST_L1_value    = 129,
    FIO_LIST_L2_value    = 130,
    FIO_LIST_L4_value    = 131,
    FIO_LIST_L8_value    = 132,
    FIO_LIST_R4_value    = 133,
    FIO_LIST_R8_value    = 134,
    FIO_LIST_R16_value   = 135,
    FIO_LIST_C4_value    = 136,
    FIO_LIST_C8_value    = 137,
    FIO_LIST_C16_value   = 138,

    /* F77 other I/O operations. */

    FIO_BACKSPACE = 139,
    FIO_CLOSE     = 140,
    FIO_DELETE    = 141,
    FIO_ENDFILE   = 142,
    FIO_FIND      = 143,
    FIO_INQUIRE   = 144,
    FIO_OPEN      = 145,
    FIO_REWIND    = 146,
    FIO_UNLOCK    = 147,
    FIO_DEFINEFILE = 148,

    /* CRAY Library IO Operations */
    FIO_CR_READ_UNFORMATTED = 149,
    FIO_CR_WRITE_UNFORMATTED = 150,
    FIO_CR_READ_FORMATTED = 151,
    FIO_CR_WRITE_FORMATTED = 152,
    FIO_CR_READ_NAMELIST = 153,
    FIO_CR_WRITE_NAMELIST = 154,
    FIO_INQLENGTH = 155,
    FIO_CR_OPEN = 156,
    FIO_CR_CLOSE = 157,
    FIO_CR_ENDFILE = 158,
    FIO_CR_REWIND = 159,
    FIO_CR_INQUIRE = 160,
    FIO_CR_BACKSPACE = 161,
    FIO_CR_BUFFERIN = 162,
    FIO_CR_BUFFEROUT = 163,

  FIOOPER_LAST = 164

} FIOOPER;

typedef enum {

  FIOITEMTYPE_NONE = 0,
  FIOITEMTYPE_FIRST = 1,

    FIT_ADDRESS4  = 1,
    FIT_ADDRESS8  = 2,
    FIT_CHARACTER = 3,
    FIT_INTEGER1  = 4,
    FIT_INTEGER2  = 5,
    FIT_INTEGER4  = 6,
    FIT_INTEGER8  = 7,
    FIT_LOGICAL1  = 8,
    FIT_LOGICAL2  = 9,
    FIT_LOGICAL4  = 10,
    FIT_LOGICAL8  = 11,
    FIT_REAL4     = 12,
    FIT_REAL8     = 13,
    FIT_REAL16    = 14,
    FIT_COMPLEX4  = 15,
    FIT_COMPLEX8  = 16,
    FIT_COMPLEX16 = 17,
    FIT_RECORD    = 18,

  FIOITEMTYPE_LAST = 18

} FIOITEMTYPE;

typedef enum {

  FIOFORMATTYPE_NONE = 0,
  FIOFORMATTYPE_FIRST = 1,

    FFT_FORMAT   = 1,
    FFT_UNFORMAT = 2,
    FFT_LIST     = 3,

  FIOFORMATTYPE_LAST = 3

} FIOFORMATTYPE;

typedef enum {

  FIOSTRUCTID_NONE = 0,
  FIOSTRUCTID_FIRST = 1,

    FID_CILIST = 1,
    FID_ICILIST = 2,
    FID_OLIST = 3,
    FID_FLIST = 4,
    FID_INLIST = 5,
    FID_ALIST = 6,
    FID_CLLIST = 7,
    FID_KEYSPEC = 8,

    FID_CRAY_CLIST = 9,
    FID_CRAY_FCD = 10,
    FID_CRAY_IOLIST = 11,
    FID_CRAY_OPENLIST = 12,
    FID_CRAY_CLOSELIST = 13,
    FID_CRAY_INQLIST = 14,
    FID_CRAY_DOPEVEC = 15,
    FID_IOSCALAR_ENTRY = 16,
    FID_IOARRAY_ENTRY = 17,
    FID_IOIMPLIEDDO_ENTRY = 18,

  FIOSTRUCTID_LAST = 18

} FIOSTRUCTID;

#define FIM_EXIST 0
#define FIM_OPENED 2
#define FIM_NUMBER 4
#define FIM_NAMED 6
#define FIM_RECL 8
#define FIM_NEXTREC 10
#define FIM_ASSOCIATEVARIABLE 12

typedef enum {

  FIOSTRUCT_NONE = 0,
  FIOSTRUCT_FIRST = 1,

    /* F77 cilist structure fields. */

    FSC_CIERR = 1,
    FSC_CIUNIT = 2,
    FSC_CIEND = 3,
    FSC_CIFMT = 4,
    FSC_CIREC = 5,
    FSC_CIMATCH = 6,
    FSC_CIKEYTYPE = 7,
    FSC_CIKEYVAL = 8,
    FSC_CIKEYID = 9,
    FSC_CINML = 10,
    FSC_CIKEYVALLEN = 11,
    FSC_CIADVANCE = 12,
    FSC_CIADVANCELEN = 13,
    FSC_CIEOR = 14,
    FSC_CISIZE = 15,
    FSC_CIVFMT = 16,
    FSC_CIVFMTFP = 17,

    /* F77 icilist structure fields. */

    FSI_ICIERR = 18,
    FSI_ICIUNIT = 19,
    FSI_ICIEND = 20,
    FSI_ICIFMT = 21,
    FSI_ICIRLEN = 22,
    FSI_ICIRNUM = 23,
    FSI_ICIVFMT = 24,
    FSI_ICIVFMTFP = 25,

    /* F77 olist structure fields. */

    FSO_OERR = 26,
    FSO_OUNIT = 27,
    FSO_OFNM = 28,
    FSO_OFNMLEN = 29,
    FSO_OSTA = 30,
    FSO_OACC = 31,
    FSO_OFM = 32,
    FSO_ORL = 33,
    FSO_OBLNK = 34,
    FSO_OCC = 35,
    FSO_OORG = 36,
    FSO_OSHARED = 37,
    FSO_OREADONLY = 38,
    FSO_ONKEYS = 39,
    FSO_OKEYS = 40,
    FSO_OASSOCV = 41,
    FSO_OMAXREC = 42,
    FSO_ODFNM = 43,
    FSO_ODFNMLEN = 44,
    FSO_ODISP = 45,
    FSO_ORECTYPE = 46,
    FSO_OCONV = 47,
    FSO_OCONVLEN = 48,
    FSO_OBUFFSIZE = 49,
    FSO_ODIRECT = 50,
    FSO_OACTION = 51,
    FSO_OACTIONLEN = 52,
    FSO_ODELIM = 53,
    FSO_ODELIMLEN = 54,
    FSO_OPAD = 55,
    FSO_OPADLEN = 56,
    FSO_OPOSITION = 57,
    FSO_OPOSITIONLEN = 58,

    /* F77 flist structure fields. */

    FSF_FERR = 59,
    FSF_FUNIT = 60,
    FSF_FREC = 61,

    /* F77 inlist structure fields. */

    FSN_INERR = 62,
    FSN_INUNIT = 63,
    FSN_INFILE = 64,
    FSN_INFILEN = 65,
    FSN_INEX = 66,
    FSN_INOPEN = 67,
    FSN_INNUM = 68,
    FSN_INNAMED = 69,
    FSN_INNAME = 70,
    FSN_INNAMLEN = 71,
    FSN_INACC = 72,
    FSN_INACCLEN = 73,
    FSN_INSEQ = 74,
    FSN_INSEQLEN = 75,
    FSN_INDIR = 76,
    FSN_INDIRLEN = 77,
    FSN_INFMT = 78,
    FSN_INFMTLEN = 79,
    FSN_INFORM = 80,
    FSN_INFORMLEN = 81,
    FSN_INUNF = 82,
    FSN_INUNFLEN = 83,
    FSN_INRECL = 84,
    FSN_INNREC = 85,
    FSN_INBLANK = 86,
    FSN_INBLANKLEN = 87,
    FSN_INDEFAULTFILE = 88,
    FSN_INDEFAULTFILELEN = 89,
    FSN_INCC = 90,
    FSN_INCCLEN = 91,
    FSN_INKEYED = 92,
    FSN_INKEYEDLEN = 93,
    FSN_INORG = 94,
    FSN_INORGLEN = 95,
    FSN_INRECORDTYPE = 96,
    FSN_INRECORDTYPELEN = 97,
    FSN_INCONV = 98,
    FSN_INCONVLEN = 99,
    FSN_INBUFFSIZE = 100,
    FSN_INACTION = 101,
    FSN_INACTIONLEN = 102,
    FSN_INDELIM = 103,
    FSN_INDELIMLEN = 104,
    FSN_INPAD = 105,
    FSN_INPADLEN = 106,
    FSN_INPOSITION = 107,
    FSN_INPOSITIONLEN = 108,
    FSN_INREAD = 109,
    FSN_INREADLEN = 110,
    FSN_INREADWRITE = 111,
    FSN_INREADWRITELEN = 112,
    FSN_INWRITE = 113,
    FSN_INWRITELEN = 114,

    /* F77 alist structure fields. */

    FSA_AERR = 115,
    FSA_AUNIT = 116,

    /* F77 cllist structure fields. */

    FSL_CLERR = 117,
    FSL_CLUNIT = 118,
    FSL_CLSTA = 119,

    /* F77 Keyspec structure fields */

    FSK_START = 120,
    FSK_END = 121,
    FSK_KEYTYPE = 122,

    /* CRAY Control List Structure Fields */

    FCR_CI_WORD1 = 123,
    FCR_CI_UNIT = 124,
    FCR_CI_IOSTAT = 125,
    FCR_CI_REC = 126,
    FCR_CI_PARSFMT = 127,
    FCR_CI_FMTSRC = 128,
    FCR_CI_ADVANCE = 129,
    FCR_CI_SIZE = 130,

    /* CRAY FCD Structure Fields */
    FCR_FCD_ADDR = 131,
    FCR_FCD_LEN = 132,

    /* Cray IOLIST header */
    FCR_IOL_HEAD = 133,

    /* Cray open descriptor fields */
    FCR_OPEN_VERSION = 134,
    FCR_OPEN_UNIT = 135,
    FCR_OPEN_IOSTAT = 136,
    FCR_OPEN_ERR = 137,
    FCR_OPEN_FILE = 138,
    FCR_OPEN_STATUS = 139,
    FCR_OPEN_ACCESS = 140,
    FCR_OPEN_FORM = 141,
    FCR_OPEN_RECL = 142,
    FCR_OPEN_BLANK = 143,
    FCR_OPEN_POSITION = 144,
    FCR_OPEN_ACTION = 145,
    FCR_OPEN_DELIM = 146,
    FCR_OPEN_PAD = 147,

   /* Cray close descriptor fields */
    FCR_CLOSE_VERSION = 148,
    FCR_CLOSE_UNIT = 149,
    FCR_CLOSE_IOSTAT = 150,
    FCR_CLOSE_ERR = 151,
    FCR_CLOSE_STATUS = 152,

   /* Cray Inquire descriptor fields */
    FCR_INQ_VERSION = 153,
    FCR_INQ_UNIT = 154,
    FCR_INQ_FILE = 155,
    FCR_INQ_IOSTAT = 156,
    FCR_INQ_ERR = 157,
    FCR_INQ_EXIST = 158,
    FCR_INQ_OPENED = 159,
    FCR_INQ_NUMBER = 160,
    FCR_INQ_NAMED = 161,
    FCR_INQ_NAME = 162,
    FCR_INQ_ACCESS = 163,
    FCR_INQ_SEQUENTIAL = 164,
    FCR_INQ_DIRECT = 165,
    FCR_INQ_FORM = 166,
    FCR_INQ_FORMATTED = 167,
    FCR_INQ_UNFORMATTED = 168,
    FCR_INQ_RECL = 169,
    FCR_INQ_NEXTREC = 170,
    FCR_INQ_BLANK = 171,
    FCR_INQ_POSITION = 172,
    FCR_INQ_ACTION = 173,
    FCR_INQ_READ = 174,
    FCR_INQ_WRITE = 175,
    FCR_INQ_READWRITE = 176,
    FCR_INQ_DELIM = 177,
    FCR_INQ_PAD = 178,

    /* Cray Dope Vector descriptor fields */
    FCR_DV_BASE_PTR = 179,
    FCR_DV_BASE_LEN = 180,
    FCR_DV_FLAG_INFO = 181,
    FCR_DV_TYPE_LEN = 182,
    FCR_DV_ORIG_BASE = 183,
    FCR_DV_ORIG_SIZE = 184,
    FCR_DV_DIM1_LB = 185,
    FCR_DV_DIM1_EXTENT = 186,
    FCR_DV_DIM1_STRIDE = 187,
    FCR_DV_DIM2_LB = 188,
    FCR_DV_DIM2_EXTENT = 189,
    FCR_DV_DIM2_STRIDE = 190,
    FCR_DV_DIM3_LB = 191,
    FCR_DV_DIM3_EXTENT = 192,
    FCR_DV_DIM3_STRIDE = 193,
    FCR_DV_DIM4_LB = 194,
    FCR_DV_DIM4_EXTENT = 195,
    FCR_DV_DIM4_STRIDE = 196,
    FCR_DV_DIM5_LB = 197,
    FCR_DV_DIM5_EXTENT = 298,
    FCR_DV_DIM5_STRIDE = 299,
    FCR_DV_DIM6_LB = 200,
    FCR_DV_DIM6_EXTENT = 201,
    FCR_DV_DIM6_STRIDE = 202,
    FCR_DV_DIM7_LB = 203,
    FCR_DV_DIM7_EXTENT = 204,
    FCR_DV_DIM7_STRIDE = 205,

       /* FID_IOSCALAR_ENTRY */
    FCR_IOSCALAR_ENTRY = 206,
    FCR_IOSCALAR_TYPE_T = 207,
    FCR_IOSCALAR_ADDR = 208,
    FCR_IOSCALAR_CHAR_LEN = 209,

      /* FID_IOARRAY_ENTRY */
    FCR_IOARRAY_ENTRY = 210,
    FCR_IOARRAY_DV_ADDR = 211,
    FCR_IOARRAY_FLAG = 212,
    FCR_IOARRAY_IDX1 = 213,
    FCR_IOARRAY_IDX2 = 214,
    FCR_IOARRAY_IDX3 = 215,
    FCR_IOARRAY_IDX4 = 216,
    FCR_IOARRAY_IDX5 = 217,
    FCR_IOARRAY_IDX6= 218,
    FCR_IOARRAY_IDX7 = 219,

    /* FID_IOIMPLIEDDO_ENTRY */
    FCR_IOIMPLIEDDO_ENTRY = 220,
    FCR_IOIMPLIEDDO_VAR_ADDR = 221,
    FCR_IOIMPLIEDDO_BEGIN_CNT = 222,
    FCR_IOIMPLIEDDO_END_CNT = 223,
    FCR_IOIMPLIEDDO_INC_CNT = 224,

  FIOSTRUCT_LAST = 224

} FIOSTRUCT;

typedef enum {

  FIOCLASS_NONE = 0,
  FIOCLASS_FIRST = 1,

    FCL_EXT_FORMATTED = 1,
    FCL_EXT_UNFORMATTED = 2,
    FCL_EXT_LIST = 3,
    FCL_EXT_NAMELIST = 4,

    FCL_INT_FORMATTED = 5,
    FCL_INT_LIST = 6,

    FCL_DIR_FORMATTED = 7,
    FCL_DIR_UNFORMATTED = 8,

  FIOCLASS_LAST = 8

} FIOCLASS;

typedef struct {
    FIOSTRUCT  first;
    FIOSTRUCT  last;
    INT16      size32;
    INT16      size64;
    const char       *name;
    const char       *name_ptr;
    const char       *name_local;
} FIOSTRUCTID_INFO;

typedef struct {
    INT16        offset32;
    INT16        type32;
    INT16        offset64;
    INT16        type64;
    FIOSTRUCTID  iostruct;
    const char  *name;
} FIOSTRUCT_INFO;

static ST * Make_IoRuntime_ST ( FIOOPER );
#define GET_RUNTIME_ST(x) (fio_sts[x] == NULL ? Make_IoRuntime_ST (x) : \
						fio_sts[x])

static void Gen_Io_Calls ( WN *, FIOOPER, WN *, WN *, INT32, WN *, WN *, WN *,
			   WN * );
#define GEN_IO_CALL_0(bl, op, ios1, ios2) \
	Gen_Io_Calls (bl, op, ios1, ios2, 0, NULL, NULL, NULL, NULL);
#define GEN_IO_CALL_1(bl, op, ios1, ios2, k1) \
	Gen_Io_Calls (bl, op, ios1, ios2, 1, k1, NULL, NULL, NULL);
#define GEN_IO_CALL_2(bl, op, ios1, ios2, k1, k2) \
	Gen_Io_Calls (bl, op, ios1, ios2, 2, k1, k2, NULL, NULL);
#define GEN_IO_CALL_3(bl, op, ios1, ios2, k1, k2, k3) \
	Gen_Io_Calls (bl, op, ios1, ios2, 3, k1, k2, k3, NULL);
#define GEN_IO_CALL_4(bl, op, ios1, ios2, k1, k2, k3, k4) \
	Gen_Io_Calls (bl, op, ios1, ios2, 4, k1, k2, k3, k4);

#define	Action(x)		(actions & (x))

#define	WN_type_pointed(x)	TY_pointed(WN_type(x))

inline WN *
WN_CreateNewLabel (void)
{
  LABEL_IDX label;
  (void) New_LABEL (CURRENT_SYMTAB, label);
  return WN_CreateLabel (label, 0, NULL);
}


INT32 mp_io;

/*  This table contains the external names of all I/O runtime routines.  */

static const char * fio_names [FIOOPER_LAST + 1] = {
    "",			/* FIOOPER_NONE */
    "s_rsfe64",		/* FIO_EXT_READ_FORMAT_start */
    "s_rsue64",		/* FIO_EXT_READ_UNFORMAT_start */
    "s_rsle64",		/* FIO_EXT_READ_LIST_start */
    "s_rsNe64",		/* FIO_EXT_READ_NAMELIST_start */
    "s_wsfe64",		/* FIO_EXT_WRITE_FORMAT_start */
    "s_wsue64",		/* FIO_EXT_WRITE_UNFORMAT_start */
    "s_wsle64",		/* FIO_EXT_WRITE_LIST_start */
    "s_wsNe64",		/* FIO_EXT_WRITE_NAMELIST_start */
    "s_xsfe64",		/* FIO_EXT_REWRITE_FORMAT_start */
    "s_xsue64",		/* FIO_EXT_REWRITE_UNFORMAT_start */
    "s_xsle64",		/* FIO_EXT_REWRITE_LIST_start */
    "e_rsfe64",		/* FIO_EXT_READ_FORMAT_end */
    "e_rsue64",		/* FIO_EXT_READ_UNFORMAT_end */
    "e_rsle64",		/* FIO_EXT_READ_LIST_end */
    "e_wsfe64",		/* FIO_EXT_WRITE_FORMAT_end */
    "e_wsue64",		/* FIO_EXT_WRITE_UNFORMAT_end */
    "e_wsle64",		/* FIO_EXT_WRITE_LIST_end */
    "e_xsfe64",		/* FIO_EXT_REWRITE_FORMAT_end */
    "e_xsue64",		/* FIO_EXT_REWRITE_UNFORMAT_end */
    "e_xsle64",		/* FIO_EXT_REWRITE_LIST_end */
    "s_rsfi64",		/* FIO_INT_READ_FORMAT_start */
    "s_rsli64",		/* FIO_INT_READ_LIST_start */
    "s_wsfi64",		/* FIO_INT_WRITE_FORMAT_start */
    "s_wsli64",		/* FIO_INT_WRITE_LIST_start */
    "e_rsfi64",		/* FIO_INT_READ_FORMAT_end */
    "e_rsli64",		/* FIO_INT_READ_LIST_end */
    "e_wsfi64",		/* FIO_INT_WRITE_FORMAT_end */
    "e_wsli64",		/* FIO_INT_WRITE_LIST_end */
    "s_rdfe64",		/* FIO_DIR_READ_FORMAT_start */
    "s_rdue64",		/* FIO_DIR_READ_UNFORMAT_start */
    "s_wdfe64",		/* FIO_DIR_WRITE_FORMAT_start */
    "s_wdue64",		/* FIO_DIR_WRITE_UNFORMAT_start */
    "e_rdfe64",		/* FIO_DIR_READ_FORMAT_end */
    "e_rdue64",		/* FIO_DIR_READ_UNFORMAT_end */
    "e_wdfe64",		/* FIO_DIR_WRITE_FORMAT_end */
    "e_wdue64",		/* FIO_DIR_WRITE_UNFORMAT_end */
    "do_fioxa4",	/* FIO_FORMAT_ADDR4_item */
    "do_fioxa8",	/* FIO_FORMAT_ADDR8_item */
    "do_fioxh1",	/* FIO_FORMAT_CHAR_item */
    "do_fioxi1",	/* FIO_FORMAT_I1_item */
    "do_fioxi2",	/* FIO_FORMAT_I2_item */
    "do_fioxi4",	/* FIO_FORMAT_I4_item */
    "do_fioxi8",	/* FIO_FORMAT_I8_item */
    "do_fioxl1",	/* FIO_FORMAT_L1_item */
    "do_fioxl2",	/* FIO_FORMAT_L2_item */
    "do_fioxl4",	/* FIO_FORMAT_L4_item */
    "do_fioxl8",	/* FIO_FORMAT_L8_item */
    "do_fioxr4",	/* FIO_FORMAT_R4_item */
    "do_fioxr8",	/* FIO_FORMAT_R8_item */
    "do_fioxr16",	/* FIO_FORMAT_R16_item */
    "do_fioxc4",	/* FIO_FORMAT_C4_item */
    "do_fioxc8",	/* FIO_FORMAT_C8_item */
    "do_fioxc16",	/* FIO_FORMAT_C16_item */
    "do_fioxa4v",	/* FIO_FORMAT_ADDR4_value */
    "do_fioxa8v",	/* FIO_FORMAT_ADDR8_value */
    "do_fioxh1v",	/* FIO_FORMAT_CHAR_value */
    "do_fioxi1v",	/* FIO_FORMAT_I1_value */
    "do_fioxi2v",	/* FIO_FORMAT_I2_value */
    "do_fioxi4v",	/* FIO_FORMAT_I4_value */
    "do_fioxi8v",	/* FIO_FORMAT_I8_value */
    "do_fioxl1v",	/* FIO_FORMAT_L1_value */
    "do_fioxl2v",	/* FIO_FORMAT_L2_value */
    "do_fioxl4v",	/* FIO_FORMAT_L4_value */
    "do_fioxl8v",	/* FIO_FORMAT_L8_value */
    "do_fioxr4v",	/* FIO_FORMAT_R4_value */
    "do_fioxr8v",	/* FIO_FORMAT_R8_value */
    "do_fioxr16v",	/* FIO_FORMAT_R16_value */
    "do_fioxc4v",	/* FIO_FORMAT_C4_value */
    "do_fioxc8v",	/* FIO_FORMAT_C8_value */
    "do_fioxc16v",	/* FIO_FORMAT_C16_value */
    "do_uioxa4",	/* FIO_UNFORMAT_ADDR4_item */
    "do_uioxa8",	/* FIO_UNFORMAT_ADDR8_item */
    "do_uioxh1",	/* FIO_UNFORMAT_CHAR_item */
    "do_uioxi1",	/* FIO_UNFORMAT_I1_item */
    "do_uioxi2",	/* FIO_UNFORMAT_I2_item */
    "do_uioxi4",	/* FIO_UNFORMAT_I4_item */
    "do_uioxi8",	/* FIO_UNFORMAT_I8_item */
    "do_uioxl1",	/* FIO_UNFORMAT_L1_item */
    "do_uioxl2",	/* FIO_UNFORMAT_L2_item */
    "do_uioxl4",	/* FIO_UNFORMAT_L4_item */
    "do_uioxl8",	/* FIO_UNFORMAT_L8_item */
    "do_uioxr4",	/* FIO_UNFORMAT_R4_item */
    "do_uioxr8",	/* FIO_UNFORMAT_R8_item */
    "do_uioxr16",	/* FIO_UNFORMAT_R16_item */
    "do_uioxc4",	/* FIO_UNFORMAT_C4_item */
    "do_uioxc8",	/* FIO_UNFORMAT_C8_item */
    "do_uioxc16",	/* FIO_UNFORMAT_C16_item */
    "do_uioxa4v",	/* FIO_UNFORMAT_ADDR4_value */
    "do_uioxa8v",	/* FIO_UNFORMAT_ADDR8_value */
    "do_uioxh1v",	/* FIO_UNFORMAT_CHAR_value */
    "do_uioxi1v",	/* FIO_UNFORMAT_I1_value */
    "do_uioxi2v",	/* FIO_UNFORMAT_I2_value */
    "do_uioxi4v",	/* FIO_UNFORMAT_I4_value */
    "do_uioxi8v",	/* FIO_UNFORMAT_I8_value */
    "do_uioxl1v",	/* FIO_UNFORMAT_L1_value */
    "do_uioxl2v",	/* FIO_UNFORMAT_L2_value */
    "do_uioxl4v",	/* FIO_UNFORMAT_L4_value */
    "do_uioxl8v",	/* FIO_UNFORMAT_L8_value */
    "do_uioxr4v",	/* FIO_UNFORMAT_R4_value */
    "do_uioxr8v",	/* FIO_UNFORMAT_R8_value */
    "do_uioxr16v",	/* FIO_UNFORMAT_R16_value */
    "do_uioxc4v",	/* FIO_UNFORMAT_C4_value */
    "do_uioxc8v",	/* FIO_UNFORMAT_C8_value */
    "do_uioxc16v",	/* FIO_UNFORMAT_C16_value */
    "do_lioxa4",	/* FIO_LIST_ADDR4_item */
    "do_lioxa8",	/* FIO_LIST_ADDR8_item */
    "do_lioxh1",	/* FIO_LIST_CHAR_item */
    "do_lioxi1",	/* FIO_LIST_I1_item */
    "do_lioxi2",	/* FIO_LIST_I2_item */
    "do_lioxi4",	/* FIO_LIST_I4_item */
    "do_lioxi8",	/* FIO_LIST_I8_item */
    "do_lioxl1",	/* FIO_LIST_L1_item */
    "do_lioxl2",	/* FIO_LIST_L2_item */
    "do_lioxl4",	/* FIO_LIST_L4_item */
    "do_lioxl8",	/* FIO_LIST_L8_item */
    "do_lioxr4",	/* FIO_LIST_R4_item */
    "do_lioxr8",	/* FIO_LIST_R8_item */
    "do_lioxr16",	/* FIO_LIST_R16_item */
    "do_lioxc4",	/* FIO_LIST_C4_item */
    "do_lioxc8",	/* FIO_LIST_C8_item */
    "do_lioxc16",	/* FIO_LIST_C16_item */
    "do_lioxa4v",	/* FIO_LIST_ADDR4_value */
    "do_lioxa8v",	/* FIO_LIST_ADDR8_value */
    "do_lioxh1v",	/* FIO_LIST_CHAR_value */
    "do_lioxi1v",	/* FIO_LIST_I1_value */
    "do_lioxi2v",	/* FIO_LIST_I2_value */
    "do_lioxi4v",	/* FIO_LIST_I4_value */
    "do_lioxi8v",	/* FIO_LIST_I8_value */
    "do_lioxl1v",	/* FIO_LIST_L1_value */
    "do_lioxl2v",	/* FIO_LIST_L2_value */
    "do_lioxl4v",	/* FIO_LIST_L4_value */
    "do_lioxl8v",	/* FIO_LIST_L8_value */
    "do_lioxr4v",	/* FIO_LIST_R4_value */
    "do_lioxr8v",	/* FIO_LIST_R8_value */
    "do_lioxr16v",	/* FIO_LIST_R16_value */
    "do_lioxc4v",	/* FIO_LIST_C4_value */
    "do_lioxc8v",	/* FIO_LIST_C8_value */
    "do_lioxc16v",	/* FIO_LIST_C16_value */
    "f_back64",		/* FIO_BACKSPACE */
    "f_clos64",		/* FIO_CLOSE */
    "f_del64",		/* FIO_DELETE */
    "f_end64",		/* FIO_ENDFILE */
    "f_find64",		/* FIO_FIND */
    "f_inqu064x",	/* FIO_INQUIRE */
    "f_open064x",	/* FIO_OPEN */
    "f_rew64",		/* FIO_REWIND */
    "f_unl64",		/* FIO_UNLOCK */
    "f_df64x",		/* FIO_DEFINEFILE */
    "_FRU",		/* FIO_CR_READ_UNFORMATTED */
    "_FWU",		/* FIO_CR_WRITE_UNFORMATTED */
    "_FRF",		/* FIO_CR_READ_FORMATTED */
    "_FWF",		/* FIO_CR_WRITE_FORMATTED */
    "_FRN",		/* FIO_CR_READ_NAMELIST */
    "_FWN",		/* FIO_CR_WRITE_NAMELIST */
    "_INQIL",		/* FIO_INQLENGTH */
    "_OPEN",		/* FIO_CR_OPEN */
    "_CLOSE",		/* FIO_CR_CLOSE */
    "_EOFW",		/* FIO_CR_ENDFILE */
    "_REWF",		/* FIO_CR_REWIND */
    "_INQUIRE",		/* IOS_CR_INQUIRE */
    "_BACK",		/* IOS_CR_BACKSPACE */
    "_BUFFERIN",	/* IOS_CR_BUFFERIN */
    "_BUFFEROUT"	/* IOS_CR_BUFFEROUT */

};

/*  This table contains pointers to the global ST entries for each of the  */
/*  I/O runtime routines.  These entries allow efficient sharing of all    */
/*  calls to a particular runtime routine.                                 */

static ST * fio_sts [FIOOPER_LAST + 1] = {
    NULL,	/* FIOOPER_NONE */
    NULL,	/* FIO_EXT_READ_FORMAT_start */
    NULL,	/* FIO_EXT_READ_UNFORMAT_start */
    NULL,	/* FIO_EXT_READ_LIST_start */
    NULL,	/* FIO_EXT_READ_NAMELIST_start */
    NULL,	/* FIO_EXT_WRITE_FORMAT_start */
    NULL,	/* FIO_EXT_WRITE_UNFORMAT_start */
    NULL,	/* FIO_EXT_WRITE_LIST_start */
    NULL,	/* FIO_EXT_WRITE_NAMELIST_start */
    NULL,	/* FIO_EXT_REWRITE_FORMAT_start */
    NULL,	/* FIO_EXT_REWRITE_UNFORMAT_start */
    NULL,	/* FIO_EXT_REWRITE_LIST_start */
    NULL,	/* FIO_EXT_READ_FORMAT_end */
    NULL,	/* FIO_EXT_READ_UNFORMAT_end */
    NULL,	/* FIO_EXT_READ_LIST_end */
    NULL,	/* FIO_EXT_WRITE_FORMAT_end */
    NULL,	/* FIO_EXT_WRITE_UNFORMAT_end */
    NULL,	/* FIO_EXT_WRITE_LIST_end */
    NULL,	/* FIO_EXT_REWRITE_FORMAT_end */
    NULL,	/* FIO_EXT_REWRITE_UNFORMAT_end */
    NULL,	/* FIO_EXT_REWRITE_LIST_end */
    NULL,	/* FIO_INT_READ_FORMAT_start */
    NULL,	/* FIO_INT_READ_LIST_start */
    NULL,	/* FIO_INT_WRITE_FORMAT_start */
    NULL,	/* FIO_INT_WRITE_LIST_start */
    NULL,	/* FIO_INT_READ_FORMAT_end */
    NULL,	/* FIO_INT_READ_LIST_end */
    NULL,	/* FIO_INT_WRITE_FORMAT_end */
    NULL,	/* FIO_INT_WRITE_LIST_end */
    NULL,	/* FIO_DIR_READ_FORMAT_start */
    NULL,	/* FIO_DIR_READ_UNFORMAT_start */
    NULL,	/* FIO_DIR_WRITE_FORMAT_start */
    NULL,	/* FIO_DIR_WRITE_UNFORMAT_start */
    NULL,	/* FIO_DIR_READ_FORMAT_end */
    NULL,	/* FIO_DIR_READ_UNFORMAT_end */
    NULL,	/* FIO_DIR_WRITE_FORMAT_end */
    NULL,	/* FIO_DIR_WRITE_UNFORMAT_end */
    NULL,	/* FIO_FORMAT_ADDR4_item */
    NULL,	/* FIO_FORMAT_ADDR8_item */
    NULL,	/* FIO_FORMAT_CHAR_item */
    NULL,	/* FIO_FORMAT_I1_item */
    NULL,	/* FIO_FORMAT_I2_item */
    NULL,	/* FIO_FORMAT_I4_item */
    NULL,	/* FIO_FORMAT_I8_item */
    NULL,	/* FIO_FORMAT_L1_item */
    NULL,	/* FIO_FORMAT_L2_item */
    NULL,	/* FIO_FORMAT_L4_item */
    NULL,	/* FIO_FORMAT_L8_item */
    NULL,	/* FIO_FORMAT_R4_item */
    NULL,	/* FIO_FORMAT_R8_item */
    NULL,	/* FIO_FORMAT_R16_item */
    NULL,	/* FIO_FORMAT_C4_item */
    NULL,	/* FIO_FORMAT_C8_item */
    NULL,	/* FIO_FORMAT_C16_item */
    NULL,	/* FIO_FORMAT_ADDR4_value */
    NULL,	/* FIO_FORMAT_ADDR8_value */
    NULL,	/* FIO_FORMAT_CHAR_value */
    NULL,	/* FIO_FORMAT_I1_value */
    NULL,	/* FIO_FORMAT_I2_value */
    NULL,	/* FIO_FORMAT_I4_value */
    NULL,	/* FIO_FORMAT_I8_value */
    NULL,	/* FIO_FORMAT_L1_value */
    NULL,	/* FIO_FORMAT_L2_value */
    NULL,	/* FIO_FORMAT_L4_value */
    NULL,	/* FIO_FORMAT_L8_value */
    NULL,	/* FIO_FORMAT_R4_value */
    NULL,	/* FIO_FORMAT_R8_value */
    NULL,	/* FIO_FORMAT_R16_value */
    NULL,	/* FIO_FORMAT_C4_value */
    NULL,	/* FIO_FORMAT_C8_value */
    NULL,	/* FIO_FORMAT_C16_value */
    NULL,	/* FIO_UNFORMAT_ADDR4_item */
    NULL,	/* FIO_UNFORMAT_ADDR8_item */
    NULL,	/* FIO_UNFORMAT_CHAR_item */
    NULL,	/* FIO_UNFORMAT_I1_item */
    NULL,	/* FIO_UNFORMAT_I2_item */
    NULL,	/* FIO_UNFORMAT_I4_item */
    NULL,	/* FIO_UNFORMAT_I8_item */
    NULL,	/* FIO_UNFORMAT_L1_item */
    NULL,	/* FIO_UNFORMAT_L2_item */
    NULL,	/* FIO_UNFORMAT_L4_item */
    NULL,	/* FIO_UNFORMAT_L8_item */
    NULL,	/* FIO_UNFORMAT_R4_item */
    NULL,	/* FIO_UNFORMAT_R8_item */
    NULL,	/* FIO_UNFORMAT_R16_item */
    NULL,	/* FIO_UNFORMAT_C4_item */
    NULL,	/* FIO_UNFORMAT_C8_item */
    NULL,	/* FIO_UNFORMAT_C16_item */
    NULL,	/* FIO_UNFORMAT_ADDR4_value */
    NULL,	/* FIO_UNFORMAT_ADDR8_value */
    NULL,	/* FIO_UNFORMAT_CHAR_value */
    NULL,	/* FIO_UNFORMAT_I1_value */
    NULL,	/* FIO_UNFORMAT_I2_value */
    NULL,	/* FIO_UNFORMAT_I4_value */
    NULL,	/* FIO_UNFORMAT_I8_value */
    NULL,	/* FIO_UNFORMAT_L1_value */
    NULL,	/* FIO_UNFORMAT_L2_value */
    NULL,	/* FIO_UNFORMAT_L4_value */
    NULL,	/* FIO_UNFORMAT_L8_value */
    NULL,	/* FIO_UNFORMAT_R4_value */
    NULL,	/* FIO_UNFORMAT_R8_value */
    NULL,	/* FIO_UNFORMAT_R16_value */
    NULL,	/* FIO_UNFORMAT_C4_value */
    NULL,	/* FIO_UNFORMAT_C8_value */
    NULL,	/* FIO_UNFORMAT_C16_value */
    NULL,	/* FIO_LIST_ADDR4_item */
    NULL,	/* FIO_LIST_ADDR8_item */
    NULL,	/* FIO_LIST_CHAR_item */
    NULL,	/* FIO_LIST_I1_item */
    NULL,	/* FIO_LIST_I2_item */
    NULL,	/* FIO_LIST_I4_item */
    NULL,	/* FIO_LIST_I8_item */
    NULL,	/* FIO_LIST_L1_item */
    NULL,	/* FIO_LIST_L2_item */
    NULL,	/* FIO_LIST_L4_item */
    NULL,	/* FIO_LIST_L8_item */
    NULL,	/* FIO_LIST_R4_item */
    NULL,	/* FIO_LIST_R8_item */
    NULL,	/* FIO_LIST_R16_item */
    NULL,	/* FIO_LIST_C4_item */
    NULL,	/* FIO_LIST_C8_item */
    NULL,	/* FIO_LIST_C16_item */
    NULL,	/* FIO_LIST_ADDR4_value */
    NULL,	/* FIO_LIST_ADDR8_value */
    NULL,	/* FIO_LIST_CHAR_value */
    NULL,	/* FIO_LIST_I1_value */
    NULL,	/* FIO_LIST_I2_value */
    NULL,	/* FIO_LIST_I4_value */
    NULL,	/* FIO_LIST_I8_value */
    NULL,	/* FIO_LIST_L1_value */
    NULL,	/* FIO_LIST_L2_value */
    NULL,	/* FIO_LIST_L4_value */
    NULL,	/* FIO_LIST_L8_value */
    NULL,	/* FIO_LIST_R4_value */
    NULL,	/* FIO_LIST_R8_value */
    NULL,	/* FIO_LIST_R16_value */
    NULL,	/* FIO_LIST_C4_value */
    NULL,	/* FIO_LIST_C8_value */
    NULL,	/* FIO_LIST_C16_value */
    NULL,	/* FIO_BACKSPACE */
    NULL,	/* FIO_CLOSE */
    NULL,	/* FIO_DELETE */
    NULL,	/* FIO_ENDFILE */
    NULL,	/* FIO_FIND */
    NULL,	/* FIO_INQUIRE */
    NULL,	/* FIO_OPEN */
    NULL,	/* FIO_REWIND */
    NULL,	/* FIO_UNLOCK */
    NULL,	/* FIO_DEFINEFILE */
    NULL,	/* FIO_CR_READ_UNFORMATTED */
    NULL,	/* FIO_CR_WRITE_UNFORMATTED */
    NULL,	/* FIO_CR_READ_FORMATTED */
    NULL,	/* FIO_CR_WRITE_FORMATTED */
    NULL,	/* FIO_CR_READ_NAMELIST */
    NULL,	/* FIO_CR_WRITE_NAMELIST */
    NULL,	/* FIO_INQLENGTH */
    NULL,	/* IOS_CR_OPEN */
    NULL,	/* IOS_CR_CLOSE */
    NULL,	/* IOS_CR_ENDFILE */
    NULL,	/* IOS_CR_REWIND */
    NULL,	/* IOS_CR_INQUIRE */
    NULL,	/* IOS_CR_BACKSPACE */
    NULL,	/* IOS_CR_BUFFERIN */
    NULL	/* IOS_CR_BUFFEROUT */
};


/*  These tables contain the FIOOPER codes for each I/O item or value.	   */
/*  The tables are indexed by the type of I/O (formatted, unformatted,	   */
/*  list-directed) and the data type of the item/value.			   */

static FIOOPER fio_item_ops  [FIOFORMATTYPE_LAST +1] [FIOITEMTYPE_LAST + 1] = {
    FIOOPER_NONE,		/* unknown, FIOITEMTYPE_NONE */
    FIOOPER_NONE,		/* unknown, FIT_ADDRESS4 */
    FIOOPER_NONE,		/* unknown, FIT_ADDRESS8 */
    FIOOPER_NONE,		/* unknown, FIT_CHARACTER */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER1 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER2 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER4 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER8 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL1 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL2 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL4 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL8 */
    FIOOPER_NONE,		/* unknown, FIT_REAL4 */
    FIOOPER_NONE,		/* unknown, FIT_REAL8 */
    FIOOPER_NONE,		/* unknown, FIT_REAL16 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX4 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX8 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* unknown, FIT_RECORD */
    FIOOPER_NONE,		/* formatted, FIOITEMTYPE_NONE */
    FIO_FORMAT_ADDR4_item,	/* formatted, FIT_ADDRESS4 */
    FIO_FORMAT_ADDR8_item,	/* formatted, FIT_ADDRESS8 */
    FIO_FORMAT_CHAR_item,	/* formatted, FIT_CHARACTER */
    FIO_FORMAT_I1_item,		/* formatted, FIT_INTEGER1 */
    FIO_FORMAT_I2_item,		/* formatted, FIT_INTEGER2 */
    FIO_FORMAT_I4_item,		/* formatted, FIT_INTEGER4 */
    FIO_FORMAT_I8_item,		/* formatted, FIT_INTEGER8 */
    FIO_FORMAT_L1_item,		/* formatted, FIT_LOGICAL1 */
    FIO_FORMAT_L2_item,		/* formatted, FIT_LOGICAL2 */
    FIO_FORMAT_L4_item,		/* formatted, FIT_LOGICAL4 */
    FIO_FORMAT_L8_item,		/* formatted, FIT_LOGICAL8 */
    FIO_FORMAT_R4_item,		/* formatted, FIT_REAL4 */
    FIO_FORMAT_R8_item,		/* formatted, FIT_REAL8 */
    FIO_FORMAT_R16_item,	/* formatted, FIT_REAL16 */
    FIO_FORMAT_C4_item,		/* formatted, FIT_COMPLEX4 */
    FIO_FORMAT_C8_item,		/* formatted, FIT_COMPLEX8 */
    FIO_FORMAT_C16_item,	/* formatted, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* formatted, FIT_RECORD */
    FIOOPER_NONE,		/* unformatted, FIOITEMTYPE_NONE */
    FIO_UNFORMAT_ADDR4_item,	/* unformatted, FIT_ADDRESS4 */
    FIO_UNFORMAT_ADDR8_item,	/* unformatted, FIT_ADDRESS8 */
    FIO_UNFORMAT_CHAR_item,	/* unformatted, FIT_CHARACTER */
    FIO_UNFORMAT_I1_item,	/* unformatted, FIT_INTEGER1 */
    FIO_UNFORMAT_I2_item,	/* unformatted, FIT_INTEGER2 */
    FIO_UNFORMAT_I4_item,	/* unformatted, FIT_INTEGER4 */
    FIO_UNFORMAT_I8_item,	/* unformatted, FIT_INTEGER8 */
    FIO_UNFORMAT_L1_item,	/* unformatted, FIT_LOGICAL1 */
    FIO_UNFORMAT_L2_item,	/* unformatted, FIT_LOGICAL2 */
    FIO_UNFORMAT_L4_item,	/* unformatted, FIT_LOGICAL4 */
    FIO_UNFORMAT_L8_item,	/* unformatted, FIT_LOGICAL8 */
    FIO_UNFORMAT_R4_item,	/* unformatted, FIT_REAL4 */
    FIO_UNFORMAT_R8_item,	/* unformatted, FIT_REAL8 */
    FIO_UNFORMAT_R16_item,	/* unformatted, FIT_REAL16 */
    FIO_UNFORMAT_C4_item,	/* unformatted, FIT_COMPLEX4 */
    FIO_UNFORMAT_C8_item,	/* unformatted, FIT_COMPLEX8 */
    FIO_UNFORMAT_C16_item,	/* unformatted, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* unformatted, FIT_RECORD */
    FIOOPER_NONE,		/* list-directed, FIOITEMTYPE_NONE */
    FIO_LIST_ADDR4_item,	/* list-directed, FIT_ADDRESS4 */
    FIO_LIST_ADDR8_item,	/* list-directed, FIT_ADDRESS8 */
    FIO_LIST_CHAR_item,		/* list-directed, FIT_CHARACTER */
    FIO_LIST_I1_item,		/* list-directed, FIT_INTEGER1 */
    FIO_LIST_I2_item,		/* list-directed, FIT_INTEGER2 */
    FIO_LIST_I4_item,		/* list-directed, FIT_INTEGER4 */
    FIO_LIST_I8_item,		/* list-directed, FIT_INTEGER8 */
    FIO_LIST_L1_item,		/* list-directed, FIT_LOGICAL1 */
    FIO_LIST_L2_item,		/* list-directed, FIT_LOGICAL2 */
    FIO_LIST_L4_item,		/* list-directed, FIT_LOGICAL4 */
    FIO_LIST_L8_item,		/* list-directed, FIT_LOGICAL8 */
    FIO_LIST_R4_item,		/* list-directed, FIT_REAL4 */
    FIO_LIST_R8_item,		/* list-directed, FIT_REAL8 */
    FIO_LIST_R16_item,		/* list-directed, FIT_REAL16 */
    FIO_LIST_C4_item,		/* list-directed, FIT_COMPLEX4 */
    FIO_LIST_C8_item,		/* list-directed, FIT_COMPLEX8 */
    FIO_LIST_C16_item,		/* list-directed, FIT_COMPLEX16 */
    FIOOPER_NONE 		/* list-directed, FIT_RECORD */
};

static FIOOPER fio_value_ops [FIOFORMATTYPE_LAST +1] [FIOITEMTYPE_LAST + 1] = {
    FIOOPER_NONE,		/* unknown, FIOITEMTYPE_NONE */
    FIOOPER_NONE,		/* unknown, FIT_ADDRESS4 */
    FIOOPER_NONE,		/* unknown, FIT_ADDRESS8 */
    FIOOPER_NONE,		/* unknown, FIT_CHARACTER */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER1 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER2 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER4 */
    FIOOPER_NONE,		/* unknown, FIT_INTEGER8 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL1 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL2 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL4 */
    FIOOPER_NONE,		/* unknown, FIT_LOGICAL8 */
    FIOOPER_NONE,		/* unknown, FIT_REAL4 */
    FIOOPER_NONE,		/* unknown, FIT_REAL8 */
    FIOOPER_NONE,		/* unknown, FIT_REAL16 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX4 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX8 */
    FIOOPER_NONE,		/* unknown, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* unknown, FIT_RECORD */
    FIOOPER_NONE,		/* formatted, FIOITEMTYPE_NONE */
    FIO_FORMAT_ADDR4_value,	/* formatted, FIT_ADDRESS4 */
    FIO_FORMAT_ADDR8_value,	/* formatted, FIT_ADDRESS8 */
    FIO_FORMAT_CHAR_value,	/* formatted, FIT_CHARACTER */
    FIO_FORMAT_I1_value,	/* formatted, FIT_INTEGER1 */
    FIO_FORMAT_I2_value,	/* formatted, FIT_INTEGER2 */
    FIO_FORMAT_I4_value,	/* formatted, FIT_INTEGER4 */
    FIO_FORMAT_I8_value,	/* formatted, FIT_INTEGER8 */
    FIO_FORMAT_L1_value,	/* formatted, FIT_LOGICAL1 */
    FIO_FORMAT_L2_value,	/* formatted, FIT_LOGICAL2 */
    FIO_FORMAT_L4_value,	/* formatted, FIT_LOGICAL4 */
    FIO_FORMAT_L8_value,	/* formatted, FIT_LOGICAL8 */
    FIO_FORMAT_R4_value,	/* formatted, FIT_REAL4 */
    FIO_FORMAT_R8_value,	/* formatted, FIT_REAL8 */
    FIO_FORMAT_R16_value,	/* formatted, FIT_REAL16 */
    FIO_FORMAT_C4_value,	/* formatted, FIT_COMPLEX4 */
    FIO_FORMAT_C8_value,	/* formatted, FIT_COMPLEX8 */
    FIO_FORMAT_C16_value,	/* formatted, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* formatted, FIT_RECORD */
    FIOOPER_NONE,		/* unformatted, FIOITEMTYPE_NONE */
    FIO_UNFORMAT_ADDR4_value,	/* unformatted, FIT_ADDRESS4 */
    FIO_UNFORMAT_ADDR8_value,	/* unformatted, FIT_ADDRESS8 */
    FIO_UNFORMAT_CHAR_value,	/* unformatted, FIT_CHARACTER */
    FIO_UNFORMAT_I1_value,	/* unformatted, FIT_INTEGER1 */
    FIO_UNFORMAT_I2_value,	/* unformatted, FIT_INTEGER2 */
    FIO_UNFORMAT_I4_value,	/* unformatted, FIT_INTEGER4 */
    FIO_UNFORMAT_I8_value,	/* unformatted, FIT_INTEGER8 */
    FIO_UNFORMAT_L1_value,	/* unformatted, FIT_LOGICAL1 */
    FIO_UNFORMAT_L2_value,	/* unformatted, FIT_LOGICAL2 */
    FIO_UNFORMAT_L4_value,	/* unformatted, FIT_LOGICAL4 */
    FIO_UNFORMAT_L8_value,	/* unformatted, FIT_LOGICAL8 */
    FIO_UNFORMAT_R4_value,	/* unformatted, FIT_REAL4 */
    FIO_UNFORMAT_R8_value,	/* unformatted, FIT_REAL8 */
    FIO_UNFORMAT_R16_value,	/* unformatted, FIT_REAL16 */
    FIO_UNFORMAT_C4_value,	/* unformatted, FIT_COMPLEX4 */
    FIO_UNFORMAT_C8_value,	/* unformatted, FIT_COMPLEX8 */
    FIO_UNFORMAT_C16_value,	/* unformatted, FIT_COMPLEX16 */
    FIOOPER_NONE,		/* unformatted, FIT_RECORD */
    FIOOPER_NONE,		/* list-directed, FIOITEMTYPE_NONE */
    FIO_LIST_ADDR4_value,	/* list-directed, FIT_ADDRESS4 */
    FIO_LIST_ADDR8_value,	/* list-directed, FIT_ADDRESS8 */
    FIO_LIST_CHAR_value,	/* list-directed, FIT_CHARACTER */
    FIO_LIST_I1_value,		/* list-directed, FIT_INTEGER1 */
    FIO_LIST_I2_value,		/* list-directed, FIT_INTEGER2 */
    FIO_LIST_I4_value,		/* list-directed, FIT_INTEGER4 */
    FIO_LIST_I8_value,		/* list-directed, FIT_INTEGER8 */
    FIO_LIST_L1_value,		/* list-directed, FIT_LOGICAL1 */
    FIO_LIST_L2_value,		/* list-directed, FIT_LOGICAL2 */
    FIO_LIST_L4_value,		/* list-directed, FIT_LOGICAL4 */
    FIO_LIST_L8_value,		/* list-directed, FIT_LOGICAL8 */
    FIO_LIST_R4_value,		/* list-directed, FIT_REAL4 */
    FIO_LIST_R8_value,		/* list-directed, FIT_REAL8 */
    FIO_LIST_R16_value,		/* list-directed, FIT_REAL16 */
    FIO_LIST_C4_value,		/* list-directed, FIT_COMPLEX4 */
    FIO_LIST_C8_value,		/* list-directed, FIT_COMPLEX8 */
    FIO_LIST_C16_value,		/* list-directed, FIT_COMPLEX16 */
    FIOOPER_NONE 		/* list-directed, FIT_RECORD */
};


/*  These variables contain a set of ST's (and their count) which   */
/*  represent user variables whose address has been passed to an    */
/*  I/O routine indirectly through a control structure.  Since the  */
/*  reference is hidden, the optimizer may not properly allocate,   */
/*  home or reload the user variable.  So, what we do is generate   */
/*  a set of 'dummy' parameter nodes on the call which list all     */
/*  such variables referenced.  Every call to Gen_Io_PutAddrWN      */
/*  saves the referenced ST in the table.  The next (first) call    */
/*  to an I/O routine has these dummy parameters appended to it.    */

static INT32     fio_dummy_max = 0;
static INT32     fio_dummy_count;
static BOOL      *fio_dummy_ref;
static ST      ** fio_dummy_st;
static TY_IDX    *fio_dummy_tyidx;
static WN_OFFSET *fio_dummy_ofst;

/*  These variables and tables contain the TY & ST pointers to the I/O	 */
/*  global types and local symbols.  The variable fio_current_symtab is  */
/*  to validate the entries in fiostruct_st.  When a new PU is first	 */
/*  entered, the ST pointers to old PU must be cleared.			 */

static TY_IDX  fioruntime_ty = (TY_IDX) 0;

static TY_IDX  fiostruct_ty [FIOSTRUCTID_LAST + 1] = {
    (TY_IDX) 0,	/* FIOSTRUCT_NONE */
    (TY_IDX) 0,	/* FID_CILIST */
    (TY_IDX) 0,	/* FID_ICILIST */
    (TY_IDX) 0,	/* FID_OLIST */
    (TY_IDX) 0,	/* FID_FLIST */
    (TY_IDX) 0,	/* FID_INLIST */
    (TY_IDX) 0,	/* FID_ALIST */
    (TY_IDX) 0,	/* FID_CLLIST */
    (TY_IDX) 0,	/* FID_KEYSPEC */
    (TY_IDX) 0,	/* FID_CRAY_CLIST */
    (TY_IDX) 0,       /* FID_CRAY_FCD */
    (TY_IDX) 0,	/* FID_CRAY_IOLIST */
    (TY_IDX) 0,	/* FID_CRAY_OPENLIST */
    (TY_IDX) 0,	/* FID_CRAY_CLOSELIST */
    (TY_IDX) 0,	/* FID_CRAY_INQLIST */
    (TY_IDX) 0,	/* FID_CRAY_DOPEVEC */
    (TY_IDX) 0,	/* FID_IOSCALAR_ENTRY */
    (TY_IDX) 0,	/* FID_IOARRAY_ENTRY */
    (TY_IDX) 0	/* FID_IOIMPLIEDDO_ENTRY */
};

static ST * fiostruct_st [FIOSTRUCTID_LAST + 1] = {
    NULL,	/* FIOSTRUCT_NONE */
    NULL,	/* FID_CILIST */
    NULL,	/* FID_ICILIST */
    NULL,	/* FID_OLIST */
    NULL,	/* FID_FLIST */
    NULL,	/* FID_INLIST */
    NULL,	/* FID_ALIST */
    NULL,	/* FID_CLLIST */
    NULL,	/* FID_KEYSPEC */
    NULL,	/* FID_CRAY_CLIST */
    NULL,       /* FID_CRAY_FCD */
    NULL,	/* FID_CRAY_IOLIST */
    NULL,	/* FID_CRAY_OPENLIST */
    NULL,	/* FID_CRAY_CLOSELIST */
    NULL,	/* FID_CRAY_INQLIST */
    NULL,	/* FID_CRAY_DOPEVEC */
    NULL,	/* FID_IOSCALAR_ENTRY */
    NULL,	/* FID_IOARRAY_ENTRY */
    NULL	/* FID_IOIMPLIEDDO_ENTRY */
};

static PU * fio_current_pu = NULL;
static PU * mp_fio_current_pu = NULL;

static PU * namelist_current_pu = NULL;
static PU * cray_iolist_current_pu = NULL;
static WN     * namelist_node_list = NULL;
static WN     * namelist_node = NULL;
static ST     * container_block_for_iolists = NULL;
static INT32  num_iolists = 0;

/*  The following two tables describe all of the information about the       */
/*  I/O runtime structures which are used to pass information between user   */
/*  code and the runtime routines.					     */
/*									     */
/*  WARNING:  It is absolutely critical that the following two structures    */
/*	      track any changes made to /usr/include/cmplrs/fioext.h.        */

static FIOSTRUCTID_INFO fiostructid_info [FIOSTRUCTID_LAST + 1] = {
    FIOSTRUCT_NONE, FIOSTRUCT_NONE,   0,    0,  "",          "",
						"",	       /* FIOSTRUCT_NONE */
    FSC_CIERR,   FSC_CISIZE,         72,  112,  ".cilist",   ".cilist_ptr",
						"_cilist",	/* FID_CILIST */
    FSI_ICIERR,  FSI_ICIRNUM,        32,   64,  ".icilist",  ".icilist_ptr",
						"_icilist",    /* FID_ICILIST */
    FSO_OERR,    FSO_OPOSITIONLEN,  136,  240,  ".olist",    ".olist_ptr",
						"_olist",	/* FID_OLIST */
    FSF_FERR,    FSF_FREC,           16,   16,  ".flist",    ".flist_ptr",
						"_flist",	/* FID_FLIST */
    FSN_INERR,   FSN_INWRITELEN,    216,  416,  ".inlist",   ".inlist_ptr",
						"_inlist",	/* FID_INLIST */
    FSA_AERR,    FSA_AUNIT,           8,    8,  ".alist",    ".alist_ptr",
						"_alist",	/* FID_ALIST */
    FSL_CLERR,   FSL_CLSTA,          16,   16,  ".cllist",   ".cllist_ptr",
						"_cllist",	/* FID_CLLIST */
    FSK_START,   FSK_KEYTYPE,        6,   6,    ".keyspec",   ".keyspec_ptr",
						"_keyspec",	/* FID_KEYSPEC*/
    FCR_CI_WORD1,  FCR_CI_SIZE,      48,  88,	".cray_clist",   ".cray_clist_ptr",
					    	"_cray_clist", /*FID_CRAY_CLIST*/
   FCR_FCD_ADDR,  FCR_FCD_LEN,       8,   16,   ".cray_fcd",    ".cray_fcd_ptr",
						"cray_fcd",   /* FID_CRAY_FCD */
   FCR_IOL_HEAD,  FCR_IOL_HEAD,	     8,   8,    ".cray_iolist",  ".cray_iolist_ptr",
						"_cray_iolist",  /*FID_CRAY_IOLIST */
   FCR_OPEN_VERSION, FCR_OPEN_PAD,  96, 184,    ".cray_open_desc", ".cray_open_desc_ptr",
						"_cray_open_desc", /* FID_CRAY_OPENLIST */
   FCR_CLOSE_VERSION, FCR_CLOSE_STATUS, 28, 48, ".cray_close_desc", ".cray_close_desc_ptr",
						"_cray_close_desc", /* FID_CRAY_CLOSELIST */
   FCR_INQ_VERSION, FCR_INQ_PAD,   172, 336,    ".cray_inq_desc",  ".cray_inq_desc_ptr",
						"_cray_inq_desc", /* FID_CRAY_INQLIST */
   FCR_DV_BASE_PTR, FCR_DV_DIM7_EXTENT, 116,    216,	".cray_dv_desc", ".cray_dv_desc_ptr",
						"_cray_dv_desc",  /* CRAY_DOPEVEC */
  FCR_IOSCALAR_ENTRY, FCR_IOSCALAR_CHAR_LEN, 24, 32, ".ioscalar_entry", "_ioscalar_ptr",
						"_ioscalar_entry",
  FCR_IOARRAY_ENTRY, FCR_IOARRAY_FLAG, 20, 24, ".ioarray_entry", "_ioarray_ptr",
						"_ioarray_entry",
  FCR_IOIMPLIEDDO_ENTRY, FCR_IOIMPLIEDDO_INC_CNT, 24, 40, ".ioimplieddo_entry", "_ioimplieddo_ptr",
						"_ioimplieddo_entry"
};

static FIOSTRUCT_INFO fiostruct_info [FIOSTRUCT_LAST + 1] = {
      0,         0,      0,         0,    FIOSTRUCTID_NONE,  "",
							/* FIOSTRUCT_NONE */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_CILIST,	"cierr",
							/* FSC_CIERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_CILIST,	"ciunit",
							/* FSC_CIUNIT */
      8,  MTYPE_I4,      8,  MTYPE_I4,    FID_CILIST,	"ciend",
							/* FSC_CIEND */
     12,  MTYPE_U4,     16,  MTYPE_U8,    FID_CILIST,	"cifmt",
							/* FSC_CIFMT */
     16,  MTYPE_I8,     24,  MTYPE_I8,    FID_CILIST,	"cirec",
							/* FSC_CIREC */
     24,  MTYPE_I4,     32,  MTYPE_I4,    FID_CILIST,	"cimatch",
							/* FSC_CIMATCH */
     28,  MTYPE_I4,     36,  MTYPE_I4,    FID_CILIST,	"cikeytype",
							/* FSC_CIKEYTYPE */
     32,  MTYPE_I4,     40,  MTYPE_I8,    FID_CILIST,	"cikeyval",
							/* FSC_CIKEYVAL */
     36,  MTYPE_I4,     48,  MTYPE_I4,    FID_CILIST,	"cikeyid",
							/* FSC_CIKEYID */
     40,  MTYPE_U4,     56,  MTYPE_U8,    FID_CILIST,	"cinml",
							/* FSC_CINML */
     44,  MTYPE_I4,     64,  MTYPE_I4,    FID_CILIST,	"cikeyvallen",
							/* FSC_CIKEYVALLEN */
     48,  MTYPE_U4,     72,  MTYPE_U8,    FID_CILIST,	"ciadvance",
							/* FSC_CIADVANCE */
     52,  MTYPE_I4,     80,  MTYPE_I4,    FID_CILIST,	"ciadvancelen",
							/* FSC_CIADVANCELEN */
     56,  MTYPE_I4,     84,  MTYPE_I4,    FID_CILIST,	"cieor",
							/* FSC_CIEOR */
     60,  MTYPE_U4,     88,  MTYPE_U8,    FID_CILIST,	"cisize",
							/* FSC_CISIZE */
     64,  MTYPE_U4,     96,  MTYPE_U8,    FID_CILIST,	"civfmt",
							/* FSC_CIVFMT */
     68,  MTYPE_U4,    104,  MTYPE_U8,    FID_CILIST,	"civfmtfp",
							/* FSC_CIVFMTFP */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_ICILIST,	"icierr",
							/* FSI_ICIERR */
      4,  MTYPE_U4,      8,  MTYPE_U8,    FID_ICILIST,	"iciunit",
							/* FSI_ICIUNIT */
      8,  MTYPE_I4,     16,  MTYPE_I4,    FID_ICILIST,	"iciend",
							/* FSI_ICIEND */
     12,  MTYPE_U4,     24,  MTYPE_U8,    FID_ICILIST,	"icifmt",
							/* FSI_ICIFMT */
     16,  MTYPE_I4,     32,  MTYPE_I8,    FID_ICILIST,	"icirlen",
							/* FSI_ICIRLEN */
     20,  MTYPE_I4,     40,  MTYPE_I8,    FID_ICILIST,	"icirnum",
							/* FSI_ICIRNUM */
     24,  MTYPE_U4,     48,  MTYPE_U8,    FID_ICILIST,	"icivfmt",
							/* FSI_ICIVFMT */
     28,  MTYPE_U4,     56,  MTYPE_U8,    FID_ICILIST,	"icivfmtfp",
							/* FSI_ICIVFMTFP */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_OLIST,	"oerr",
							/* FSO_OERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_OLIST,	"ounit",
							/* FSO_OUNIT */
      8,  MTYPE_U4,      8,  MTYPE_U8,    FID_OLIST,	"ofnm",
							/* FSO_OFNM */
     12,  MTYPE_I4,     16,  MTYPE_I4,    FID_OLIST,	"ofnmlen",
							/* FSO_OFNMLEN */
     16,  MTYPE_U4,     24,  MTYPE_U8,    FID_OLIST,	"osta",
							/* FSO_OSTA */
     20,  MTYPE_U4,     32,  MTYPE_U8,    FID_OLIST,	"oacc",
							/* FSO_OACC */
     24,  MTYPE_U4,     40,  MTYPE_U8,    FID_OLIST,	"ofm",
							/* FSO_OFM */
     28,  MTYPE_I4,     48,  MTYPE_I8,    FID_OLIST,	"orl",
							/* FSO_ORL */
     32,  MTYPE_U4,     56,  MTYPE_U8,    FID_OLIST,	"oblnk",
							/* FSO_OBLNK */
     36,  MTYPE_U4,     64,  MTYPE_U8,    FID_OLIST,	"occ",
							/* FSO_OCC */
     40,  MTYPE_U4,     72,  MTYPE_U8,    FID_OLIST,	"oorg",
							/* FSO_OORG */
     44,  MTYPE_I4,     80,  MTYPE_I4,    FID_OLIST,	"oshared",
							/* FSO_OSHARED */
     48,  MTYPE_I4,     84,  MTYPE_I4,    FID_OLIST,	"oreadonly",
							/* FSO_OREADONLY */
     52,  MTYPE_I4,     88,  MTYPE_I4,    FID_OLIST,	"onkeys",
							/* FSO_ONKEYS */
     56,  MTYPE_U4,     96,  MTYPE_U8,    FID_OLIST,	"okeys",
							/* FSO_OKEYS */
     60,  MTYPE_U4,    104,  MTYPE_U8,    FID_OLIST,	"oassocv",
							/* FSO_OASSOCV */
     64,  MTYPE_I8,    112,  MTYPE_I8,    FID_OLIST,	"omaxrec",
							/* FSO_OMAXREC */
     72,  MTYPE_U4,    120,  MTYPE_U8,    FID_OLIST,	"odfnm",
							/* FSO_ODFNM */
     76,  MTYPE_I4,    128,  MTYPE_I4,    FID_OLIST,	"odfnmlen",
							/* FSO_ODFNMLEN */
     80,  MTYPE_U4,    136,  MTYPE_U8,    FID_OLIST,	"odisp",
							/* FSO_ODISP */
     84,  MTYPE_U4,    144,  MTYPE_U8,    FID_OLIST,	"orectype",
							/* FSO_ORECTYPE */
     88,  MTYPE_U4,    152,  MTYPE_U8,    FID_OLIST,	"oconv",
							/* FSO_OCONV */
     92,  MTYPE_I4,    160,  MTYPE_I4,    FID_OLIST,	"oconvlen",
							/* FSO_OCONVLEN */
     96,  MTYPE_I4,    164,  MTYPE_I4,    FID_OLIST,	"obuffsize",
							/* FSO_OBUFFSIZE */
    100,  MTYPE_I4,    168,  MTYPE_I4,    FID_OLIST,	"odirect",
							/* FSO_ODIRECT */
    104,  MTYPE_U4,    176,  MTYPE_U8,    FID_OLIST,	"oaction",
							/* FSO_OACTION */
    108,  MTYPE_I4,    184,  MTYPE_I4,    FID_OLIST,	"oactionlen",
							/* FSO_OACTIONLEN */
    112,  MTYPE_U4,    192,  MTYPE_U8,    FID_OLIST,	"odelim",
							/* FSO_ODELIM */
    116,  MTYPE_I4,    200,  MTYPE_I4,    FID_OLIST,	"odelimlen",
							/* FSO_ODELIMLEN */
    120,  MTYPE_U4,    208,  MTYPE_U8,    FID_OLIST,	"opad",
							/* FSO_OPAD */
    124,  MTYPE_I4,    216,  MTYPE_I4,    FID_OLIST,	"opadlen",
							/* FSO_OPADLEN */
    128,  MTYPE_U4,    224,  MTYPE_U8,    FID_OLIST,	"oposition",
							/* FSO_OPOSITION */
    132,  MTYPE_I4,    232,  MTYPE_I4,    FID_OLIST,	"opositionlen",
							/* FSO_OPOSITIONLEN */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_FLIST,	"ferr",
							/* FSF_FERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_FLIST,	"funit",
							/* FSF_FUNIT */
      8,  MTYPE_I8,      8,  MTYPE_I8,    FID_FLIST,	"frec",
							/* FSF_FREC */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_INLIST,	"inerr",
							/* FSN_INERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_INLIST,	"inunit",
							/* FSN_INUNIT */
      8,  MTYPE_U4,      8,  MTYPE_U8,    FID_INLIST,	"infile",
							/* FSN_INFILE */
     12,  MTYPE_I4,     16,  MTYPE_I4,    FID_INLIST,	"infilen",
							/* FSN_INFILEN */
     16,  MTYPE_U4,     24,  MTYPE_U8,    FID_INLIST,	"inex",
							/* FSN_INEX */
     20,  MTYPE_U4,     32,  MTYPE_U8,    FID_INLIST,	"inopen",
							/* FSN_INOPEN */
     24,  MTYPE_U4,     40,  MTYPE_U8,    FID_INLIST,	"innum",
							/* FSN_INNUM */
     28,  MTYPE_U4,     48,  MTYPE_U8,    FID_INLIST,	"innamed",
							/* FSN_INNAMED */
     32,  MTYPE_U4,     56,  MTYPE_U8,    FID_INLIST,	"inname",
							/* FSN_INNAME */
     36,  MTYPE_I4,     64,  MTYPE_I4,    FID_INLIST,	"innamlen",
							/* FSN_INNAMLEN */
     40,  MTYPE_U4,     72,  MTYPE_U8,    FID_INLIST,	"inacc",
							/* FSN_INACC */
     44,  MTYPE_I4,     80,  MTYPE_I4,    FID_INLIST,	"inacclen",
							/* FSN_INACCLEN */
     48,  MTYPE_U4,     88,  MTYPE_U8,    FID_INLIST,	"inseq",
							/* FSN_INSEQ */
     52,  MTYPE_I4,     96,  MTYPE_I4,    FID_INLIST,	"inseqlen",
							/* FSN_INSEQLEN */
     56,  MTYPE_U4,    104,  MTYPE_U8,    FID_INLIST,	"indir",
							/* FSN_INDIR */
     60,  MTYPE_I4,    112,  MTYPE_I4,    FID_INLIST,	"indirlen",
							/* FSN_INDIRLEN */
     64,  MTYPE_U4,    120,  MTYPE_U8,    FID_INLIST,	"infmt",
							/* FSN_INFMT */
     68,  MTYPE_I4,    128,  MTYPE_I4,    FID_INLIST,	"infmtlen",
							/* FSN_INFMTLEN */
     72,  MTYPE_U4,    136,  MTYPE_U8,    FID_INLIST,	"inform",
							/* FSN_INFORM */
     76,  MTYPE_I4,    144,  MTYPE_I4,    FID_INLIST,	"informlen",
							/* FSN_INFORMLEN */
     80,  MTYPE_U4,    152,  MTYPE_U8,    FID_INLIST,	"inunf",
							/* FSN_INUNF */
     84,  MTYPE_I4,    160,  MTYPE_I4,    FID_INLIST,	"inunflen",
							/* FSN_INUNFLEN */
     88,  MTYPE_U4,    168,  MTYPE_U8,    FID_INLIST,	"inrecl",
							/* FSN_INRECL */
     92,  MTYPE_U4,    176,  MTYPE_U8,    FID_INLIST,	"innrec",
							/* FSN_INNREC */
     96,  MTYPE_U4,    184,  MTYPE_U8,    FID_INLIST,	"inblank",
							/* FSN_INBLANK */
    100,  MTYPE_I4,    192,  MTYPE_I4,    FID_INLIST,	"inblanklen",
							/* FSN_INBLANKLEN */
    104,  MTYPE_U4,    200,  MTYPE_U8,    FID_INLIST,	"indefaultfile",
							/* FSN_INDEFAULTFILE */
    108,  MTYPE_I4,    208,  MTYPE_I4,    FID_INLIST,	"indefaultfilelen",
						      /* FSN_INDEFAULTFILELEN */
    112,  MTYPE_U4,    216,  MTYPE_U8,    FID_INLIST,	"incc",
							/* FSN_INCC */
    116,  MTYPE_I4,    224,  MTYPE_I4,    FID_INLIST,	"incclen",
							/* FSN_INCCLEN */
    120,  MTYPE_U4,    232,  MTYPE_U8,    FID_INLIST,	"inkeyed",
							/* FSN_INKEYED */
    124,  MTYPE_I4,    240,  MTYPE_I4,    FID_INLIST,	"inkeyedlen",
							/* FSN_INKEYEDLEN */
    128,  MTYPE_U4,    248,  MTYPE_U8,    FID_INLIST,	"inorg",
							/* FSN_INORG */
    132,  MTYPE_I4,    256,  MTYPE_I4,    FID_INLIST,	"inorglen",
							/* FSN_INORGLEN */
    136,  MTYPE_U4,    264,  MTYPE_U8,    FID_INLIST,	"inrecordtype",
							/* FSN_INRECORDTYPE */
    140,  MTYPE_I4,    272,  MTYPE_I4,    FID_INLIST,	"inrecordtypelen",
						       /* FSN_INRECORDTYPELEN */
    144,  MTYPE_U4,    280,  MTYPE_U8,    FID_INLIST,	"inconv",
							/* FSN_INCONV */
    148,  MTYPE_I4,    288,  MTYPE_I4,    FID_INLIST,	"inconvlen",
							/* FSN_INCONVLEN */
    152,  MTYPE_U4,    296,  MTYPE_U8,    FID_INLIST,	"inbuffsize",
							/* FSN_INBUFFSIZE */
    156,  MTYPE_U4,    304,  MTYPE_U8,    FID_INLIST,	"inaction",
							/* FSN_INACTION */
    160,  MTYPE_I4,    312,  MTYPE_I4,    FID_INLIST,	"inactionlen",
							/* FSN_INACTIONLEN */
    164,  MTYPE_U4,    320,  MTYPE_U8,    FID_INLIST,	"indelim",
							/* FSN_INDELIM */
    168,  MTYPE_I4,    328,  MTYPE_I4,    FID_INLIST,	"indelimlen",
							/* FSN_INDELIMLEN */
    172,  MTYPE_U4,    336,  MTYPE_U8,    FID_INLIST,	"inpad",
							/* FSN_INPAD */
    176,  MTYPE_I4,    344,  MTYPE_I4,    FID_INLIST,	"inpadlen",
							/* FSN_INPADLEN */
    180,  MTYPE_U4,    352,  MTYPE_U8,    FID_INLIST,	"inposition",
							/* FSN_INPOSITION */
    184,  MTYPE_I4,    360,  MTYPE_I4,    FID_INLIST,	"inpositionlen",
							/* FSN_INPOSITIONLEN */
    188,  MTYPE_U4,    368,  MTYPE_U8,    FID_INLIST,	"inread",
							/* FSN_INREAD */
    192,  MTYPE_I4,    376,  MTYPE_I4,    FID_INLIST,	"inreadlen",
							/* FSN_INREADLEN */
    196,  MTYPE_U4,    384,  MTYPE_U8,    FID_INLIST,	"inreadwrite",
							/* FSN_INREADWRITE */
    200,  MTYPE_I4,    392,  MTYPE_I4,    FID_INLIST,	"inreadwritelen",
							/* FSN_INREADWRITELEN */
    204,  MTYPE_U4,    400,  MTYPE_U8,    FID_INLIST,	"inwrite",
							/* FSN_INWRITE */
    208,  MTYPE_I4,    408,  MTYPE_I4,    FID_INLIST,	"inwritelen",
							/* FSN_INWRITELEN */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_ALIST,	"aerr",
							/* FSA_AERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_ALIST,	"aunit",
							/* FSA_AUNIT */

      0,  MTYPE_I4,      0,  MTYPE_I4,    FID_CLLIST,	"clerr",
							/* FSL_CLERR */
      4,  MTYPE_I4,      4,  MTYPE_I4,    FID_CLLIST,	"clunit",
							/* FSL_CLUNIT */
      8,  MTYPE_U4,      8,  MTYPE_U8,    FID_CLLIST,	"clsta",
							/* FSL_CLSTA */
	
      0, MTYPE_I2,	 0,  MTYPE_I2,    FID_KEYSPEC, "keystart",
							/* FSK_KEYSTART */
      2, MTYPE_I2,       2,  MTYPE_I2,    FID_KEYSPEC, "keyend",
							/* FSK_KEYEND */
      4, MTYPE_I2,       4,  MTYPE_I2,    FID_KEYSPEC, "keytype",
							/* FSK_KEYTYPE */

      0, MTYPE_U8,       0,  MTYPE_U8,    FID_CRAY_CLIST, "cray_ci_word1",
							/* FCR_CI_WORD1 */
      8, MTYPE_M,        8,  MTYPE_M,     FID_CRAY_CLIST, "cray_ci_unit",
							/* FCR_CI_UNIT */
     16, MTYPE_U4,      24,  MTYPE_U8,    FID_CRAY_CLIST, "cray_ci_iostat",
							/* FCR_CI_IOSTAT */
     20, MTYPE_U4,      32,  MTYPE_U8,    FID_CRAY_CLIST, "cray_ci_rec",
							/* FCR_CI_REC */
     24, MTYPE_U4,      40,  MTYPE_U8,    FID_CRAY_CLIST, "cray_ci_parsfmt",
							/* FCR_CI_PARSFMT */
     28, MTYPE_M,       48,  MTYPE_M,     FID_CRAY_CLIST, "cray_ci_fmtsrc",
							/* FCR_CI_FMTSRC */
     36, MTYPE_M,       64,  MTYPE_M,     FID_CRAY_CLIST, "cray_ci_advance",
							/* FCR_CI_ADVANCE */
     44, MTYPE_U4,      80, MTYPE_U8,     FID_CRAY_CLIST, "cray_ci_size",
							/* FCR_CI_SIZE */

      0, MTYPE_U4,	0, MTYPE_U8,	  FID_CRAY_FCD,   "cray_fcd_addr",
							/* FCR_FCD_ADDR */
      4, MTYPE_U4,      8, MTYPE_U8,      FID_CRAY_FCD,   "cray_fcd_len",
							/* FCR_FCD_LEN */

      0, MTYPE_U8,	0, MTYPE_U8,	  FID_CRAY_IOLIST, "cray_iol_head",
							/* FCR_IOL_HEAD */

      0, MTYPE_U8,	0, MTYPE_U8,      FID_CRAY_OPENLIST, "cray_open_version",
							/* FCR_OPEN_VERSION */
      8, MTYPE_U4,	8, MTYPE_U8,      FID_CRAY_OPENLIST, "cray_open_unit",
							/* FCR_OPEN_UNIT */
     12, MTYPE_U4,     16, MTYPE_U8,      FID_CRAY_OPENLIST, "cray_open_iostat",
							/* FCR_OPEN_IOSTAT */
     16, MTYPE_U4,     24, MTYPE_U8,      FID_CRAY_OPENLIST, "cray_open_err",
							/* FCR_OPEN_ERR */
     20, MTYPE_M,      32, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_file",
							/* FCR_OPEN_FILE */
     28, MTYPE_M,      48, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_status",
							/* FCR_OPEN_STATUS */
     36, MTYPE_M,      64, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_access",
							/* FCR_OPEN_ACCESS */
     44, MTYPE_M,      80, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_form",
							/* FCR_OPEN_FORM */
     52, MTYPE_U4,     96, MTYPE_U8,      FID_CRAY_OPENLIST, "cray_open_recl",
							/* FCR_OPEN_RECL */
     56, MTYPE_M,     104, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_blank",
							/* FCR_OPEN_BLANK */
     64, MTYPE_M,     120, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_position",
							/* FCR_OPEN_POSITION */
     72, MTYPE_M,     136, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_action",
							/* FCR_OPEN_ACTION */
     80, MTYPE_M,     152, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_delim",
							/* FCR_OPEN_DELIM */
     88, MTYPE_M,     168, MTYPE_M,       FID_CRAY_OPENLIST, "cray_open_pad",
							/* FCR_OPEN_PAD */

     0, MTYPE_U8,      0, MTYPE_U8,       FID_CRAY_CLOSELIST, "cray_close_version",
							/* FCR_CLOSE_VERSION */
     8, MTYPE_U4,      8, MTYPE_U8,       FID_CRAY_CLOSELIST, "cray_close_unit",
							/* FCR_CLOSE_UNIT */
    12, MTYPE_U4,     16, MTYPE_U8,       FID_CRAY_CLOSELIST, "cray_close_iostat",
							/* FCR_CLOSE_IOSTAT */
    16, MTYPE_U4,     24, MTYPE_U8,       FID_CRAY_CLOSELIST, "cray_close_err",
							/* FCR_CLOSE_ERR */
    20, MTYPE_M,      32, MTYPE_M,        FID_CRAY_CLOSELIST, "cray_close_status",
							/* FCR_CLOSE_STATUS */

     0, MTYPE_U8,      0, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_version",
							/* FCR_INQ_VERSION */	
     8, MTYPE_U4,      8, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_unit",
							/* FCR_INQ_UNIT */
    12, MTYPE_M,      16, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_file",
							/* FCR_INQ_FILE */
    20, MTYPE_U4,     32, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_iostat",
							/* FCR_INQ_IOSTAT */
    24, MTYPE_U4,     40, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_err",
							/* FCR_INQ_ERR */
    28, MTYPE_U4,     48, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_exist",
							/* FCR_INQ_EXIST */
    32, MTYPE_U4,     56, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_opened",
							/* FCR_INQ_OPENED */
    36, MTYPE_U4,     64, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_number",
							/* FCR_INQ_NUMBER */
    40, MTYPE_U4,     72, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_named",
							/* FCR_INQ_NAMED */
    44, MTYPE_M,      80, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_name",
							/* FCR_INQ_NAME */
    52, MTYPE_M,      96, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_access",
							/* FCR_INQ_ACCESS */
    60, MTYPE_M,     112, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_sequential",
							/* FCR_INQ_SEQUENTIAL */
    68, MTYPE_M,     128, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_direct",
							/* FCR_INQ_DIRECT */
    76, MTYPE_M,     144, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_form",
							/* FCR_INQ_FORM */
    84, MTYPE_M,     160, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_formatted",
							/* FCR_INQ_FORMATTED */
    92, MTYPE_M,     176, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_unformatted",
							/* FCR_INQ_UNFORMATTED */
   100, MTYPE_U4,    192, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_recl",
							/* FCR_INQ_RECL */
   104, MTYPE_U4,    200, MTYPE_U8,       FID_CRAY_INQLIST,   "cray_inq_nextrec",
							/* FCR_INQ_NEXTREC */
   108, MTYPE_M,     208, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_blank",
							/* FCR_INQ_BLANK */
   116, MTYPE_M,     224, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_position",
							/* FCR_INQ_POSITION */
   124, MTYPE_M,     240, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_action",
							/* FCR_INQ_ACTION */
   132, MTYPE_M,     256, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_read",
							/* FCR_INQ_READ */
   140, MTYPE_M,     272, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_write",
							/* FCR_INQ_WRITE */
   148, MTYPE_M,     288, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_readwrite",
							/* FCR_INQ_READWRITE */
   156, MTYPE_M,     304, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_delim",
							/* FCR_INQ_DELIM */
   164, MTYPE_M,     320, MTYPE_M,        FID_CRAY_INQLIST,   "cray_inq_pad",
							/* FCR_INQ_PAD */

     0, MTYPE_U4,      0, MTYPE_U8,	FID_CRAY_DOPEVEC, "cray_dv_base_addr",
							/* FCR_DV_BASE_PTR */
     4, MTYPE_I4,      8, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_base_len",
							/* FCR_DV_BASE_LEN */
     8, MTYPE_U8,      16, MTYPE_U8,	FID_CRAY_DOPEVEC, "cray_dv_flag_info",
							/* FCR_DV_FLAG_INFO */
     16, MTYPE_U8,     24, MTYPE_U8,	FID_CRAY_DOPEVEC, "cray_dv_type_len",
							/* FCR_DV_TYPE_LEN */
     24, MTYPE_U4,     32, MTYPE_U8,	FID_CRAY_DOPEVEC, "cray_dv_orig_base",
							/* FCR_DV_ORIG_BASE */
     28, MTYPE_I4,     40, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_orig_size",
							/* FCR_DV_ORIG_SIZE */
     32, MTYPE_I4,     48, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim1_lb",
							/* FCR_DV_DIM1_LB */
     36, MTYPE_I4,     56, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim1_extent",
							/* FCR_DV_DIM1_EXTENT */
     40, MTYPE_I4,     64, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim1_stride",
							/* FCR_DV_DIM1_STRIDE */
     44, MTYPE_I4,     72, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim2_lb",
							/* FCR_DV_DIM2_LB */
     48, MTYPE_I4,     80, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim2_extent",
							/* FCR_DV_DIM2_EXTENT */
     52, MTYPE_I4,     88, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim2_stride",
							/* FCR_DV_DIM2_STRIDE */
     56, MTYPE_I4,     96, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim3_lb",
							/* FCR_DV_DIM3_LB */
     60, MTYPE_I4,     104, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim3_extent",
							/* FCR_DV_DIM3_EXTENT */
     64, MTYPE_I4,     112, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim3_stride",
							/* FCR_DV_DIM3_STRIDE */
     68, MTYPE_I4,     120, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim4_lb",
							/* FCR_DV_DIM4_LB */
     72, MTYPE_I4,     128, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim4_extent",
							/* FCR_DV_DIM4_EXTENT */
     76, MTYPE_I4,     136, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim4_stride",
							/* FCR_DV_DIM4_STRIDE */
     80, MTYPE_I4,     144, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim5_lb",
							/* FCR_DV_DIM5_LB */
     84, MTYPE_I4,     152, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim5_extent",
							/* FCR_DV_DIM5_EXTENT */
     88, MTYPE_I4,     160, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim5_stride",
							/* FCR_DV_DIM5_STRIDE */
     92, MTYPE_I4,     168, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim6_lb",
							/* FCR_DV_DIM6_LB */
     96, MTYPE_I4,     176, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim6_extent",
							/* FCR_DV_DIM6_EXTENT */
     100, MTYPE_I4,    184, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim6_stride",
							/* FCR_DV_DIM6_STRIDE */
     104, MTYPE_I4,    192, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim7_lb",
							/* FCR_DV_DIM7_LB */
     108, MTYPE_I4,    200, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim7_extent",
							/* FCR_DV_DIM7_EXTENT */
     112, MTYPE_I4,    208, MTYPE_I8,	FID_CRAY_DOPEVEC, "cray_dv_dim7_stride",
							/* FCR_DV_DIM7_STRIDE */
       /* FCR_IOSCALAR_ENTRY */
       0, MTYPE_I8,	0, MTYPE_I8,    FID_IOSCALAR_ENTRY, "scalar_header",
       8, MTYPE_I8,	8, MTYPE_I8,    FID_IOSCALAR_ENTRY, "scalar_type_t",
      16, MTYPE_U4,    16, MTYPE_U8,	FID_IOSCALAR_ENTRY, "iovar_address",
      20, MTYPE_I4,    24, MTYPE_I8,    FID_IOSCALAR_ENTRY, "scalar_char_len",

      /* FCR_IOARRAY_ENTRY */
       0, MTYPE_I8,     0, MTYPE_I8,    FID_IOARRAY_ENTRY, "array_header",
       8, MTYPE_U4,     8, MTYPE_U8,    FID_IOARRAY_ENTRY, "dope_vector_addr",
      12, MTYPE_I8,    16, MTYPE_I8,    FID_IOARRAY_ENTRY, "array_flag",
      20, MTYPE_U4,    24, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_1",
      24, MTYPE_U4,    32, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_2",
      28, MTYPE_U4,    40, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_3",
      32, MTYPE_U4,    48, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_4",
      36, MTYPE_U4,    56, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_5",
      40, MTYPE_U4,    64, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_6",
      44, MTYPE_U4,    72, MTYPE_U8,    FID_IOARRAY_ENTRY, "array_indx_7",

      /* FCR_IOIMPLIEDDO_ENTRY */
       0, MTYPE_I8,     0, MTYPE_I8,    FID_IOIMPLIEDDO_ENTRY, "implieddo_header",
       8, MTYPE_U4,     8, MTYPE_U8,    FID_IOIMPLIEDDO_ENTRY, "loop_var_addr",
      12, MTYPE_U4,    16, MTYPE_U8,    FID_IOIMPLIEDDO_ENTRY, "begin_cnt_addr",
      16, MTYPE_U4,    24, MTYPE_U8,    FID_IOIMPLIEDDO_ENTRY, "end_cnt__addr",
      20, MTYPE_U4,    32, MTYPE_U8,    FID_IOIMPLIEDDO_ENTRY, "inc_cnt_addr",
};


#define FIO_OFFSET(i)	((Pointer_Size == 4) ? fiostruct_info[i].offset32 \
						: fiostruct_info[i].offset64)
#define FIO_TYPE(i)	((Pointer_Size == 4) ? fiostruct_info[i].type32 \
						: fiostruct_info[i].type64)
#define FIO_SIZE(i)	((Pointer_Size == 4) ? fiostructid_info[i].size32 \
						: fiostructid_info[i].size64)
#define Int_Type        ((Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8)
#define OPC_IntWord	((Pointer_Size == 4) ? OPC_I4INTCONST : OPC_I8INTCONST)


static WN *extract_calls ( WN *, WN * );
static INT32
lower_f77_io_items ( WN *, WN *, WN *, WN*, WN*, FIOOPER, BOOL, INT32 *,
			INT32, INT32 );
static void
lower_f77_record_items (WN *, WN *, WN *, WN *, WN *, WN *,
			FIOOPER, ST **, TY_IDX*, INT32 *, FLD_HANDLE&,
			INT32 *, INT32 *, INT32, TY_IDX , INT64);

/*  The following table gives the I/O mask code for each of the basic types.  */

static INT32 fio_maskcode [MTYPE_LAST + 1] = {
  0,	/* MTYPE_UNKNOWN */
  0,	/* MTYPE_B */
  1,	/* MTYPE_I1 */
  2,	/* MTYPE_I2 */
  0,	/* MTYPE_I4 */
  3,	/* MTYPE_I8 */
  1,	/* MTYPE_U1 */
  2,	/* MTYPE_U2 */
  0,	/* MTYPE_U4 */
  3,	/* MTYPE_U8 */
  0,	/* MTYPE_F4 */
  0,	/* MTYPE_F8 */
  0,	/* MTYPE_F10 */
  0,	/* MTYPE_F16 */
  0,	/* MTYPE_STRING */
  0,	/* MTYPE_FQ */
  0,	/* MTYPE_M */
  0,	/* MTYPE_C4 */
  0,	/* MTYPE_C8 */
  0,	/* MTYPE_CQ */
  0	/* MTYPE_V */
};



typedef enum {
   ARB_UBOUND,
   ARB_LBOUND,
   ARB_STRIDE
} arb_enum;

static WN *
Get_ST_Ldid(ST_IDX st)
{
   WN *r;
   TY_IDX ty;
   ty = ST_type(st);
   r = WN_Ldid(TY_mtype(ty),0,st,ty);
   return (r);
}

static void io_set_addr_passed_flag(ST *st) {
   if ( WHIRL_Addr_Passed_On ) 
      Set_ST_addr_passed(st);
   if (WHIRL_Addr_Saved_For_Passed_On)
      Set_ST_addr_saved(st);
}

static void io_set_addr_saved_flag(ST *st) {
   if (WHIRL_Addr_Saved_On)
      Set_ST_addr_saved(st);
}

// Utility to return as a WHIRL node the value of an ARB
static WN *
Get_ARB_WN(const ARB_HANDLE arb, arb_enum whattoget)
{
   switch (whattoget) {
    case ARB_UBOUND:
       if (ARB_const_ubnd(arb)) {
	  return (WN_Intconst(MTYPE_I8,ARB_ubnd_val(arb)));
       } else {
	  return (Get_ST_Ldid(ARB_ubnd_var(arb)));
       }
    case ARB_LBOUND:
       if (ARB_const_lbnd(arb)) {
	  return (WN_Intconst(MTYPE_I8,ARB_lbnd_val(arb)));
       } else {
	  return (Get_ST_Ldid(ARB_lbnd_var(arb)));
       }
    case ARB_STRIDE:
       if (ARB_const_stride(arb)) {
	  return (WN_Intconst(MTYPE_I8,ARB_stride_val(arb)));
       } else {
	  return (Get_ST_Ldid(ARB_stride_var(arb)));
       }
   }
   Fail_FmtAssertion("bad arguments to Get_ARB_WN");
   return NULL;
}


static TY_IDX
Make_Simple_Array_Type (const char *name, INT32 n_elems, TY_IDX elem_ty)
{
    TY_IDX ty_idx;
    TY& ty = New_TY (ty_idx);

    UINT32 elem_size = TY_size (elem_ty);

    TY_Init (ty, elem_size * n_elems, KIND_ARRAY, MTYPE_UNKNOWN,
	     Save_Str (name));
    Set_TY_etype (ty, elem_ty);

    ARB_HANDLE arb = New_ARB ();
    ARB_Init (arb, 0, n_elems - 1, elem_size);
    Set_ARB_first_dimen (arb);
    Set_ARB_last_dimen (arb);

    Set_TY_arb (ty, arb);
    Set_TY_align_exp (ty_idx, TY_align_exp (elem_ty));
    return ty_idx;
}


static void
Alloc_More_For_Dummy_Array(void)
{
   if (fio_dummy_max == 0) {
      fio_dummy_max = 32;
      fio_dummy_ref = (BOOL *)  malloc (sizeof(BOOL) * fio_dummy_max);
      fio_dummy_st = (ST **) malloc (sizeof(ST *) * fio_dummy_max);
      fio_dummy_tyidx = (TY_IDX *) malloc (sizeof(TY_IDX *) * fio_dummy_max);
      fio_dummy_ofst = (WN_OFFSET *) malloc (sizeof(WN_OFFSET) *
                                                    fio_dummy_max);
   } else {
      fio_dummy_max = fio_dummy_max + (fio_dummy_max >> 1);
      fio_dummy_ref = (BOOL *) realloc (fio_dummy_ref,
                                    sizeof(BOOL) * fio_dummy_max);
      fio_dummy_st = (ST **) realloc (fio_dummy_st,
                                      sizeof(ST *) * fio_dummy_max);
      fio_dummy_tyidx = (TY_IDX *) realloc (fio_dummy_tyidx,
                                      sizeof(TY_IDX *) * fio_dummy_max);
      fio_dummy_ofst = (WN_OFFSET *) realloc (fio_dummy_ofst,
                                     sizeof(WN_OFFSET) * fio_dummy_max);
   }
}

static void
Add_To_Dummy_List(WN *dummy) 
{
    while ((WN_operator(dummy) != OPR_LDA) && 
	   (WN_operator(dummy) != OPR_LDID))
	if ((WN_operator(dummy) == OPR_ADD) &&
	    (WN_operator(WN_kid0(dummy)) == OPR_MPY))
	    dummy = WN_kid1(dummy);
	else
	    dummy = WN_kid0(dummy);

    if (fio_dummy_count == fio_dummy_max)
	Alloc_More_For_Dummy_Array();

    ST* st = WN_st (dummy);
    fio_dummy_ref[fio_dummy_count]    = WN_operator(dummy) == OPR_LDA;
    fio_dummy_st[fio_dummy_count]     = st;
    fio_dummy_tyidx[fio_dummy_count]  = WN_ty(dummy);
    fio_dummy_ofst[fio_dummy_count++] = WN_offset(dummy);
    if (ST_class(st) == CLASS_VAR)
       io_set_addr_saved_flag(st);
}

static char *
Remove_Trailing_Blanks(char *s)
{
  char *c;
  c = s + strlen(s) - 1;
  while (*c == ' ')
    c--;
  c++;
  *c = '\0';
  return s;
}
/*===================================================
 *
 * Find_array_TY
 *
 * Given a TY, find its array ty, ie: the bottom
 * of any KIND_POINTERS to a KIND_ARRAY etc...
 * Given a scalar, just hand it back..
 *
 ====================================================
*/
extern TY_IDX 
Find_array_TY(TY_IDX  ty)
{
  TY_IDX  rty ;

  switch(TY_kind(ty)) {
  case KIND_ARRAY:
  case KIND_SCALAR:
  case KIND_STRUCT:
  case KIND_FUNCTION:
  case KIND_VOID:
    rty = ty;
    break;

  case KIND_POINTER:
    rty = Find_array_TY(TY_pointed(Ty_Table[ty])) ;
    break;

  default:
    DevAssert((0),("Odd array ty"));
    break;
  }

  return(rty);
}

/*===================================================
 *
 * Find_scalar_TY
 *
 * Given a TY, find its scalar ty, ie: the bottom
 * of any KIND_ARRAYs
 *
 ====================================================
*/
extern TY_IDX 
Find_scalar_TY(TY_IDX  ty)
{
  TY_IDX  rty ;

  switch(TY_kind(ty)) {

  case KIND_VOID:
  case KIND_SCALAR:
  case KIND_STRUCT:
  case KIND_POINTER:
  case KIND_FUNCTION:
    rty = ty;
    break;

  case KIND_ARRAY:
    rty = Find_scalar_TY(TY_AR_etype(ty)) ;
    break;

  default:
    DevAssert((0),("Odd ty"));
    break;
  }

  return(rty);
}
/* ====================================================================
 *
 * WN *create_lda_of_temp(WN *block, WN *tree, TY_IDX ty)
 *
 * Store tree into a temp and create lda of that temp
 *
 * ==================================================================== */
static WN *create_lda_of_temp(WN *block, WN *tree, TY_IDX ty)
{
      TYPE_ID   type;
      ST  *st;
      WN  *stid;

      Is_True((WN_operator_is(tree, OPR_PARM)==FALSE),("bad parm"));
      /*
       *  store value to an addressible temporary, and take the address of that
       */
      if (ty)
         type = TY_mtype(ty);
      else
         type = WN_rtype(tree);

      st = Gen_Temp_Symbol( MTYPE_To_TY(type), "complex-temp-expr");
      stid = WN_Stid (type, 0, st, ST_type(st), WN_COPY_Tree(tree));
      WN_INSERT_BlockLast(block, stid);

      return WN_Lda(Pointer_type, WN_store_offset(stid), st);
}

/* ====================================================================
 *
 * WN *create_pointer_to_node(WN *block, WN *tree, TY_IDX ty, BOOL deref)
 *
 * Return the address of tree.
 *	for ILOAD and LDID use address directly if desc type and rtype is same.
 *	if a ty is passed, rtype is determined from this ty rather than from the
 *	rtype of the whirl node we are dealing with
 *
 *      for ISTORE we can use the address directly
 *      for LDID/STID we can create an LDA
 *      otherwise we need to create an addressable temp, store to it
 *      and try again.
 *	
 *	When 'deref' is FALSE, then for an LDA and ARRAY node, we will still
 *	store into a temp and pass the address of the temp; this is needed for
 *	example in write(*,*) loc(x); we don't want the library to dereference
 *	x, we just want to print the address of x.
 * ==================================================================== */

static WN *create_pointer_to_node(WN *block, WN *tree, TY_IDX ty, BOOL deref)
{

  TYPE_ID rtype;
  TYPE_ID desc;
  WN  *add;

  /* Need to deal with the INTRINSIC_OP */
  tree = extract_calls( block, tree );
  switch (WN_operator(tree))
  {
  case OPR_ILOAD:
      if (ty)
	rtype = TY_mtype( ty );
     else
        rtype = WN_rtype(tree);

      desc = WN_desc(tree);

      if (rtype == desc) {
	 add = WN_Add(Pointer_Mtype,WN_kid0(tree),
		      WN_Intconst(Pointer_Mtype,WN_load_offset(tree)));
	 return (add);
      } else {
         return create_lda_of_temp(block, tree, ty); 
      }
    
  case OPR_ISTORE:
    return WN_kid1(tree);

  case OPR_LDID:
      if ((Language == LANG_F77) && TY_is_character(Ty_Table [ty])) {
	 /* When the incoming tree is an INTRIN_OP to handle concatenation
	    of strings extract_calls could return with an LDID that is the 
	    address of a string; PV 550165; in this situation we can just
	    return what extract_calls gave us*/
	 return tree;
      }
      if (ty)
	rtype = TY_mtype( ty );
      else
        rtype = WN_rtype(tree);

      desc = WN_desc(tree);
      if ((ST_class(WN_st(tree)) != CLASS_PREG) &&
	  (rtype == desc)) {
         return WN_Lda(Pointer_type, WN_load_offset(tree), WN_st(tree));
      } else {
         return create_lda_of_temp(block, tree, ty);
      }

  case OPR_STID:
    return WN_Lda(Pointer_type, WN_store_offset(tree), WN_st(tree));

  case OPR_ARRAY:
  case OPR_LDA:
    if (deref) {
       return tree;
    } else {
      return create_lda_of_temp(block, tree, ty);
    }

  case OPR_ADD:

#ifdef KEY
    // Under -IPA, different PUs may have different src_lang, but the driver
    // always passes -LANG:=ansi_c
    if (!PU_f90_lang(Get_Current_PU())) {
#else
    if (Language != LANG_F90) {
#endif // KEY
       /* If address expression, return the tree */
  
       if (WN_opcode(tree) == OPC_U4ADD || WN_opcode(tree) == OPC_U8ADD)
           return(tree);
       else 
           return create_lda_of_temp(block, tree, ty);
    } else {
       return create_lda_of_temp(block, tree, ty);
    }

  case OPR_INTRINSIC_OP:
    {
      /* If it is an INTRINSIC_OP to get a VALTMP then cannot load that
      ** value and store in a temp.  Need to get kid(0) instead.
      */
    }
  default:
       return create_lda_of_temp(block, tree, ty);  
  }
}


/*===================================================
 *
 * Type_is_logical
 *
 * return T if this is a logical TY
 *
 ====================================================
*/
extern BOOL
Type_is_logical(TY_IDX  ty)
{
  TY_IDX  ts ;

#ifdef KEY
  // Under -IPA, different PUs may have different src_lang, but the driver
  // always passes -LANG:=ansi_c
  if (!PU_f90_lang(Get_Current_PU())) {
#else
  if (Language != LANG_F90) {
#endif // KEY
    ts = Find_array_TY(ty);
    ts = Find_scalar_TY(ts);
    return (TY_is_logical(Ty_Table[ts]));
  } else {
    return (TY_is_logical(Ty_Table[ty]));
  }
}

/*  This routine creates a global type for the selected I/O structure.    */
/*  These struct types are only created on demand when they are needed    */
/*  to support the creation of a local struct.  Once created, they have   */
/*  persistence throughout compiling the entire source file.  All of the  */
/*  information needed to create the structs (size, name, fields, etc)    */
/*  is contained in the fiostructid_info and fiostruct_info tables.       */ 
/*  This routine also creates a global type which is a pointer to the     */
/*  appropriate structure type.						  */

static void Make_IoStruct_TY ( FIOSTRUCTID id )
{
    INT32 /* FIOSTRUCT */ i;
    FIOSTRUCT first = fiostructid_info[id].first;
    FIOSTRUCT last = fiostructid_info[id].last;

    /* Create the structure TY and fill in the appropriate fields. */

    /*
     ** If it is an iolist then make the type local so not to polute the
     ** global space with thousands of local types
     */

    TY_IDX ty_idx;
    TY& ty = New_TY (ty_idx);

    if (id == FID_CRAY_IOLIST) {
	sprintf( seq_buff, "_%d", local_sequence );
	TY_Init (ty, (Pointer_Size == 4 ? fiostructid_info[id].size32 :
		      fiostructid_info[id].size64),
	         KIND_STRUCT, MTYPE_M,
		 Save_Str2 (fiostructid_info[id].name, seq_buff));
    } else {
	TY_Init (ty, (Pointer_Size == 4 ? fiostructid_info[id].size32 :
		      fiostructid_info[id].size64),
		 KIND_STRUCT, MTYPE_M,
		 Save_Str (fiostructid_info[id].name));
    }
    
    if (current_io_library == IOLIB_MIPS) {
	if (id != FID_KEYSPEC)
	    Set_TY_align (ty_idx, MTYPE_align_req(MTYPE_I8));
	else
	    Set_TY_align (ty_idx, MTYPE_align_req(MTYPE_I2));
    } else {
	Set_TY_align (ty_idx, MTYPE_align_req(MTYPE_I8));
    }

    /* Create all of the subordinate field entries. */
    FLD_HANDLE fld;

    for (i = first; i <= last; ++i) {
	fld = New_FLD ();
	if (Pointer_Size == 4) {
	    FLD_Init (fld, Save_Str (fiostruct_info[i].name),
		      Be_Type_Tbl (fiostruct_info[i].type32),
		      fiostruct_info[i].offset32);
	} else
	    FLD_Init (fld, Save_Str (fiostruct_info[i].name),
		      Be_Type_Tbl (fiostruct_info[i].type64),
		      fiostruct_info[i].offset64);
    }
    Set_FLD_last_field (fld);
    Set_TY_fld (ty, FLD_HANDLE (fld.Idx () - (last - first)));


    /* Create the structure pointer TY and fill in the appropriate fields. */

    /*
     ** If it is an iolist then make the type local so not to polute the
     ** global space with thousands of local types
     */
    TY_IDX tyx_idx;
    TY& tyx = New_TY (tyx_idx);
    TY_Init (tyx, Pointer_Size, KIND_POINTER, Pointer_Mtype,
	     Save_Str (fiostructid_info[id].name_ptr));
    Set_TY_align (tyx_idx, Pointer_Size);
    Set_TY_pointed (tyx, ty_idx);
    fiostruct_ty[id] = ty_idx;
}


/*  This routine returns the ST for a local structure of the requested type.  */
/*  If this is the first time in this PU, all saved ST pointers are cleared.  */
/*  If the ST already exists, just return it.  If the ST doesn't exist then   */
/*  create it.  If the global type doesn't exist, create that.  Finally, if   */
/*  requested, generate code to clear the structure.			      */

static ST * Get_IoStruct_ST ( WN * block, FIOSTRUCTID id, BOOL clear )
{
  INT32 i;
  ST * st;
  WN * wn;

  /*  Clear ST pointers in fiostruct_st when first entering a new PU.  */

  if (Current_pu != fio_current_pu) {
    local_sequence = 1;
    fio_current_pu = Current_pu;
    for (i=FIOSTRUCTID_FIRST; i<=FIOSTRUCTID_LAST; i++)
      fiostruct_st[i] = NULL;
  }

  /*  If a (previously created) ST doesn't exist, create it.  */

  if ((st = fiostruct_st[id]) == NULL) {

    /*  Get the global struct TY if it exists, otherwise create it.  */
    /* For iolist, always needs a new type since the struture is 
    ** different each time */
    if ((id == FID_CRAY_IOLIST) || 
	(fiostruct_ty[id] == (TY_IDX) 0))
      Make_IoStruct_TY ( id );

    /*  Create the local ST and enter into the local symbol table and save  */
    /*  the pointer.  */

    st = New_ST ();
    ST_Init (st, 0, CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, fiostruct_ty[id]);
    io_set_addr_passed_flag(st);
    /* 
    ** Never set fiostruct_st for the following cases which use a
    ** local ST each time
    */
    if ((id != FID_CRAY_FCD) && (id != FID_CRAY_IOLIST) 
	&& (id != FID_CRAY_DOPEVEC)) {
	Set_ST_name_idx (st, Save_Str ( fiostructid_info[id].name_local ));
	fiostruct_st[id] = st;
    } else {
	sprintf( seq_buff, "_%d", local_sequence++ );
	Set_ST_name_idx (st, Save_Str2( fiostructid_info[id].name_local,
				     seq_buff ));
    }
  }

  /*  Clear the structure if requested.  */

  if (clear) {
    if (Pointer_Size == 4)
      wn = WN_CreateMstore ( 0, TY_pointer(ST_type(st)),
	   WN_CreateIntconst ( OPC_U8INTCONST, 0 ),
	   WN_CreateLda ( OPC_U4LDA, 0, TY_pointer(ST_type(st)), st),
	   WN_CreateIntconst ( OPC_U4INTCONST, TY_size(ST_type(st)) ));
    else
      wn = WN_CreateMstore ( 0, TY_pointer(ST_type(st)),
	   WN_CreateIntconst ( OPC_U8INTCONST, 0 ),
	   WN_CreateLda ( OPC_U8LDA, 0, TY_pointer(ST_type(st)), st),
	   WN_CreateIntconst ( OPC_U8INTCONST, TY_size(ST_type(st)) ));
    WN_INSERT_BlockLast ( block, wn );
  }

  return (st);
}

/*  This routine returns an ST for a local array of key struct elements.      */
/*  If the global key struct type doesn't exist, create it.  No attempt to    */
/*  share local arrays is made, usually they will be of different sizes       */
/*  anyway.                                                                   */

static ST * Get_KeyStruct_ST ( INT32 nitems )
{
  INT32 nelem = nitems/3;
  char	ty_name[30];
  char	ptr_name[30];
  char	st_name[30];

  /*  If the global key struct TY doesn't exist, create it.  */

  if (fiostruct_ty[FID_KEYSPEC] == (TY_IDX) 0)
    Make_IoStruct_TY ( FID_KEYSPEC );

  /*  Create a local TY which is an array of key structs.  */

  TY_IDX ty_idx;
  TY& ty = New_TY (ty_idx);
  sprintf( ty_name, ".key_type.%d", nelem );
  TY_Init (ty, nelem * TY_size(fiostruct_ty[FID_KEYSPEC]), KIND_ARRAY,
	   MTYPE_UNKNOWN, Save_Str ( ty_name ));
  Set_TY_etype (ty, fiostruct_ty[FID_KEYSPEC]);
  Set_TY_align (ty_idx, MTYPE_align_req(MTYPE_I8));

  ARB_HANDLE arb = New_ARB ();
  ARB_Init (arb, 0, nelem - 1, TY_size(fiostruct_ty[FID_KEYSPEC]));
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);

  Set_TY_arb (ty, arb);


  /*  Create the local array pointer TY. */

  TY_IDX tyx_idx;
  TY& tyx = New_TY (tyx_idx);
  sprintf( ptr_name, ".key_pointer.%d", nelem );
  TY_Init (tyx, Pointer_Size, KIND_POINTER, Pointer_Mtype,
	   Save_Str (ptr_name));
  Set_TY_align (tyx_idx, Pointer_Size);

  /*  Complete the TY entries and enter them in the local symbol table.  */

  Set_TY_pointed (tyx, ty_idx);

  /*  Create the local ST.  */

  ST* st = New_ST ();
  sprintf( st_name, ".key_array.%d", nelem );
  ST_Init (st, Save_Str (st_name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
	   ty_idx);

  io_set_addr_saved_flag(st);
  fiostruct_st[ FID_KEYSPEC ] = st;

  return (st);
}


/*  This routines creates a local ST for a variable of type pointer to       */
/*  save the the UNIT structure pointer. between the different steps of a    */
/*  multiple-step I/O operations such as READ/WRITE.   This is needed        */
/*  in the case of MP I/O as we can no longer depend on a static variable    */
/*  in the runtime library to preserve the setup of the initialization       */
/*  step.                                                                    */

static ST * Get_UnitPointer_ST ( void )
{
  static ST    * mpunit_ptr_var;

  if (Current_pu  != mp_fio_current_pu) {
    mp_fio_current_pu = Current_pu;
    mpunit_ptr_var = NULL;
  }

  if (mpunit_ptr_var)
    return( mpunit_ptr_var );

  static TY_IDX mpunit_ptr_ty_idx;

  if (mpunit_ptr_ty_idx == 0) {
      TY& mpunit_ptr_ty = New_TY (mpunit_ptr_ty_idx);
      TY_Init (mpunit_ptr_ty, Pointer_Size, KIND_POINTER, Pointer_Mtype, 
	       Save_Str( ".mpunit_pointer" ));
      Set_TY_pointed (mpunit_ptr_ty, Be_Type_Tbl(MTYPE_I4));
      Set_TY_align (mpunit_ptr_ty_idx, Pointer_Size);
  }

  mpunit_ptr_var = New_ST ();
  ST_Init (mpunit_ptr_var, Save_Str (".mpunit_var"), CLASS_VAR,
	   SCLASS_AUTO, EXPORT_LOCAL, mpunit_ptr_ty_idx);
  io_set_addr_saved_flag(mpunit_ptr_var);
  return mpunit_ptr_var;
}


static void
Init_fioruntime_ty ()
{
    TY& ty = New_TY (fioruntime_ty);
    TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, Save_Str (".ioruntime"));
    Set_TY_align (fioruntime_ty, 1);
#ifdef KEY
    ty.Set_pu_flag(TY_HAS_PROTOTYPE);
#endif

    TYLIST_IDX tylist_idx;
    TYLIST& tylist = New_TYLIST (tylist_idx);
    Set_TY_tylist (ty, tylist_idx);
    Set_TYLIST_type (tylist, Be_Type_Tbl ( MTYPE_I4 ));
    Set_TYLIST_type (New_TYLIST (tylist_idx), 0);

    TY_IDX tyx_idx;
    TY& tyx = New_TY (tyx_idx);
    TY_Init (tyx, Pointer_Size, KIND_POINTER, Pointer_Mtype,
	     Save_Str (".ioruntime_ptr"));
    Set_TY_pointed (tyx, fioruntime_ty);
} // Init_fioruntime_ty


/*  This routine creates a global ST for the requested I/O runtime routine.  */
/*  If the global type of the runtime routines does not exist, it is	     */
/*  created.								     */

static ST * Make_IoRuntime_ST ( FIOOPER op )
{
  ST *st;
  char mpname[ 40 ];
  INT32 i;

  /*  Do a little validation of the requested routine.  */

  if (op == FIOOPER_NONE)
    Fail_FmtAssertion("Make_IoRuntime_ST:"
		      " null runtime operation in I/O processing");


  /*  If the global type doesn't exist, create it and it's pointer type.  */

  if (fioruntime_ty == (TY_IDX) 0) {
      Init_fioruntime_ty ();
  }
  
  /*  Create the ST, fill in all appropriate fields */

  fio_sts[op] = st = New_ST (GLOBAL_SYMTAB);
  STR_IDX st_name;
  if (mp_io && (current_io_library == IOLIB_MIPS)) {
    strcpy (mpname, fio_names[op] );
    i = strlen( mpname );
    mpname[i] = '_'; mpname[i+1] = 'm'; mpname[i+2] = 'p'; mpname[i+3] = '\0';
    st_name = Save_Str ( mpname );
  } else {
    if (op == FIO_CR_OPEN && Language == LANG_F77 && current_io_library == IOLIB_CRAY)
       st_name = Save_Str ("_OPENF77");
    else
       st_name = Save_Str ( fio_names[op] );
  }

  PU_IDX pu_idx;
  PU&    pu = New_PU (pu_idx);

  PU_Init (pu, fioruntime_ty, CURRENT_SYMTAB);

  ST_Init (st, st_name, CLASS_FUNC, SCLASS_EXTERN, EXPORT_PREEMPTIBLE,
	   TY_IDX (pu_idx));

  return st;

}


/*  This routine takes an ST and generates an OPC_U4LDA or OPC_U8LDA  */
/*  node depending on pointer size.                                   */

static WN * Make_IoAddr_WN ( ST * st )
{
    WN * wn;
    TY_IDX ty = ST_type (st);
    TY_IDX ty_ptr = TY_pointer (ty);

    if (ty_ptr == (TY_IDX) 0) {
	ty_ptr = Make_Pointer_Type (ty);
    }

    if (Pointer_Size == 4)
	wn = WN_CreateLda ( OPC_U4LDA, 0, ty_ptr, st);
    else
	wn = WN_CreateLda ( OPC_U8LDA, 0, ty_ptr, st);

    io_set_addr_passed_flag(st);

    return (wn);
}

/*  This routine takes a WN and generates a PARM node over it.  */

static WN * Gen_Parm_WN ( WN * wn )
{
    OPCODE opc = WN_opcode(wn);
    TYPE_ID rtype = OPCODE_rtype(opc);

    if (rtype == MTYPE_U4 || rtype == MTYPE_U8) {
      wn = WN_CreateParm (rtype, wn,
                          (OPCODE_has_1ty(opc))?WN_ty(wn):Be_Type_Tbl(rtype),
                          WN_PARM_BY_REFERENCE );
    }
    else
      wn = WN_CreateParm ( rtype, wn, Be_Type_Tbl(rtype), WN_PARM_BY_VALUE );

    return (wn);
}


/*  This routine is the main work horse of the I/O lowerer.  Given       */
/*  information about an I/O runtime routine to be called, it generates  */
/*  the call and any required handling of the status return.  Note the   */
/*  special handling of dummy parms.  Any user variable/constant whose   */
/*  address is put in an I/O control structure could be referenced or    */
/*  modified without the knowledge of the optimizer.  So a set of dummy  */
/*  parms is created for each such variable and attached to each I/O     */
/*  call. A similar problem exists for namelist I/O where the variables  */
/*  being referenced are not "visible" in the I/O statement.  Here again */
/*  dummy parms are created for these variables.			 */

static void Gen_Io_Calls ( WN * block, FIOOPER op, WN * iostat1, WN * iostat2,
			   INT32 kids, WN * kid0, WN * kid1, WN * kid2,
			   WN * kid3 )
{
  WN * wn;
  INT32 i;
  INT32 fio_namelist_count = (namelist_node) ? WN_kid_count(namelist_node) - 2
					     : 0;

  /*  Create the appropriate call node.  */

  if (iostat1 != NULL || iostat2 != NULL || 
      op == FIO_INQLENGTH)
    wn = WN_Create ( OPC_I4CALL, kids + fio_dummy_count + fio_namelist_count);
  else
    wn = WN_Create ( OPC_VCALL, kids + fio_dummy_count + fio_namelist_count);
  WN_st_idx(wn) = ST_st_idx (GET_RUNTIME_ST ( op ));
  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Parm_Mod ( wn );
  WN_Set_Call_Parm_Ref ( wn );
  if (op == FIO_CR_READ_NAMELIST || op == FIO_CR_WRITE_NAMELIST) 
     WN_Set_Call_Non_Parm_Ref( wn );

  /*  Fill in any or all function arguments to the call.  */

  switch (kids) {
    case 4:
      WN_kid3(wn) = Gen_Parm_WN ( kid3 );
    case 3:
      WN_kid2(wn) = Gen_Parm_WN ( kid2 );
    case 2:
      WN_kid1(wn) = Gen_Parm_WN ( kid1 );
    case 1:
      WN_kid0(wn) = Gen_Parm_WN ( kid0 );
    case 0:
      break;
    default:
      Fail_FmtAssertion("Gen_Io_Calls: unexpected number of kids (%d)"
			" in I/O processing", kids);
  }

  /*  Generate any dummy parms needed and attach to the call.  */

  for (i = 0; i < fio_dummy_count; i++) {
    if (ST_class(fio_dummy_st[i]) == CLASS_VAR) {
      io_set_addr_passed_flag(fio_dummy_st[i]);
    }
    if (fio_dummy_ref[i])
      WN_kid(wn, kids++) = WN_CreateParm ( Pointer_type,
					   WN_Lda ( Pointer_type,
						    fio_dummy_ofst[i],
						    fio_dummy_st[i] ),
					   Be_Type_Tbl(Pointer_type),
					   WN_PARM_BY_REFERENCE |
					   WN_PARM_DUMMY );
    else
      WN_kid(wn, kids++) = WN_CreateParm ( Pointer_type,
					   WN_Ldid ( Pointer_type,
						     fio_dummy_ofst[i],
						     fio_dummy_st[i],
						     fio_dummy_tyidx[i]),
					   Be_Type_Tbl(Pointer_type),
					   WN_PARM_BY_REFERENCE |
					   WN_PARM_DUMMY );
  }

  for (i = 0; i < fio_namelist_count; i++) {
    WN_kid(wn, kids++) = WN_CreateParm ( Pointer_type,
			     WN_COPY_Tree (WN_kid0(WN_kid(namelist_node, i+2))),
					 Be_Type_Tbl(Pointer_type),
					 WN_PARM_BY_REFERENCE |
					 WN_PARM_DUMMY );
  }

  /*  Insert the call into the statement block being created for the I/O  */
  /*  statement.							  */

  WN_INSERT_BlockLast ( block, wn );

  /*  Add any statements needed to handle the status result.  */

 if (iostat1 != NULL || iostat2 != NULL)
    if (iostat2 == NULL)
      WN_INSERT_BlockLast ( block, WN_COPY_Tree ( iostat1 ) );
    else {
      WN_INSERT_BlockLast ( block, iostat2 );
      if (iostat1 != NULL) {
	WN_DELETE_Tree ( iostat1 );
      }
    }
}


/* This is copied from file libI77/fmt.h */
#define TYUNKNOWN 0
#define TYADDR 1
#define TYBYTE 2
#define TYSHORT 3
#define TYINT 4
#define TYLONGLONG 5
#define TYREAL 6
#define TYDREAL 7
#define TYCOMPLEX 8
#define TYDCOMPLEX 9
#define TYLOGICAL1 10
#define TYLOGICAL2 11
#define TYLOGICAL4 12
#define TYLOGICAL8 13
#define TYCHAR 14
#define TYSUBR 15
#define TYSTRUCTURE 16
#define TYNML 17
#define TYQUAD 18
#define TYQUADCOMPLEX 19
#define TYQUADLONG 20


static void Gen_Impld_Io_Calls ( WN * block, FIOFORMATTYPE form, 
			   FIOITEMTYPE type, WN * iostat1, WN * impld_item, 
			   WN *arr_item, WN * mp_unit_ptr)
/* 
  This function generates a single call to the runtime routine do_xxxx_1dim()
  for implied-do loop instead of having a loop generated to do I/O on one 
  element at a time.  The implied-DO loop must satisfy the following
  conditions to be converted:
        - must have one and only one I/O item in the implied-do list
        - the item must be an array
        - every subscript of the array must be a simple variable
        - the leftmost index must be the same as the implied-do variable
        and all other indices must be different from the leftmost index.
        E.g:  (ARR(I,J), I=1,10) will be converted whereas
        (ARR(I,J), J=1,10) and (ARR(I,I), I=1,10) will not.
	- the array is not character type
	- there can be no nested calls in the subscript expression

  These cases are not converted into a single call (could be done later but
  not very important):
	- the index is not a simple variable but an expression,
	For example: (ARR(I+1), I = 1,10)
	
  The following arguments will be passed to the do_xxxx_1dim() runtime 
  routine.  Thay are all passed by reference unless otherwise indicated:
        1) the item type (for formatted I/O only)
        2) address of the array with the leftmost index replaced by
        constant 1.  Note that this could cause a problem once the
        -check_bounds option is implemented for implied-do I/O if it
	is done after the I/O lowering phase for this reference without
	considering the implied-do bounds.
        3) The implied-do loop variable:  Its value will be modified
        appropriately when the I/O is completed.   This is the main
	reason why we need to implement the implied-do optimization
	this way as the implied-do loop variable has to be defined
	once execution goes into the implied-do I/O.  This is especially
	important for READ statement where people deliberately read a
	very large array with implied-do loop and expect the
	do loop variable to hold the number of elements sucessfully read
	into the array before the end-of-record.   We can't set this
	implied-do variable if we treat the implied-do loop as an array
	as in fcom.
        4) the lower-bound of the do-loop
        5) the upper bound of the do-loop
        6) the step size
        7) the length of one element in the implied-do (passed by value)
        8) the size of the implied-do variable (passed by value)

  CAVEAT:
  -------
  - As noted in argument 2) above.  In the case of an array declared
  as ARR(20:30) for example and used in an implied-do loop 
  (ARR(I), I = 20,30) then this function will make a reference to
  ARR(1) and passes to the I/O library to be used as the base to
  calculate the offset.  If the check_bounds implementation generates
  code to check on this reference then it will cause an error where there
  should be none.
  - An array fio_types is created to map the FE types to the types
  defined in the Fortran I/O library.   Changes to the type table either
  from the FE or from the I/O library could cause serious regression if
  this table is not updated.
*/
{
  WN * wn, *dovar;
  INT32 i, size;
  char impld_name[48];
  ST *tmp_st;
  static ST * impld_fio_sts [FIOFORMATTYPE_LAST + 1];
  static const char *impld_fio_names [ FIOFORMATTYPE_LAST + 1] =
	{ "", "do_fio64", "do_uio64", "do_Lio64" };
  INT32	nkids;
  TY_IDX aty;
  ST *st;
  static INT32 fio_types[FIOITEMTYPE_LAST + 1];

  /* Update this static table at runtime to lessen the chance of regression
  ** in case the type tables are changed either in the FE or in the I/O
  ** library
  */
  if (fio_types[ FIT_COMPLEX16 ] == 0) {
    fio_types[FIOITEMTYPE_NONE] = TYUNKNOWN;	
    fio_types[FIT_ADDRESS4] = TYADDR;		
    fio_types[FIT_ADDRESS8] = TYADDR;	
    fio_types[FIT_CHARACTER] = TYCHAR;
    fio_types[FIT_INTEGER1] = TYBYTE;
    fio_types[FIT_INTEGER2] = TYSHORT;	
    fio_types[FIT_INTEGER4] = TYINT;
    fio_types[FIT_INTEGER8] = TYLONGLONG;	
    fio_types[FIT_LOGICAL1] = TYLOGICAL1;
    fio_types[FIT_LOGICAL2] = TYLOGICAL2;
    fio_types[FIT_LOGICAL4] = TYLOGICAL4;
    fio_types[FIT_LOGICAL8] = TYLOGICAL8;
    fio_types[FIT_REAL4] = TYREAL;	
    fio_types[FIT_REAL8] = TYDREAL;
    fio_types[FIT_REAL16] = TYQUAD;
    fio_types[FIT_COMPLEX4] = TYCOMPLEX;
    fio_types[FIT_COMPLEX8] = TYDCOMPLEX;	
    fio_types[FIT_COMPLEX16] = TYQUADCOMPLEX;
    fio_types[FIT_RECORD] = TYSTRUCTURE;
  }

  /*  Create the appropriate call node.  */
  wn = WN_Create ( OPC_I4CALL, 7 + (mp_unit_ptr ? 1 : 0)
			+ (form == FFT_UNFORMAT ? 0 : 1) );
  if (impld_fio_sts[form] == NULL) {
  
    /*  Do a little validation of the requested routine.  */
  
    /*  If the global type doesn't exist, create it and it's pointer type.  */

    if (fioruntime_ty == (TY_IDX) 0) {

      Init_fioruntime_ty ();

    }

    /*  Create the ST, fill in all appropriate fields and enter into the  */
    /*  global symbol table. */

    strcpy( impld_name, impld_fio_names[form] );
    i = strlen( impld_name );
    if (mp_unit_ptr) {
      impld_name[i] = '_'; impld_name[i+1] = 'm'; impld_name[i+2] = 'p'; 
      strcpy( &impld_name[i+3], "_1dim" );
    } else {
      strcpy( &impld_name[i], "_1dim" );
    }
    impld_fio_sts[form] = st = New_ST ( GLOBAL_SYMTAB );

    PU_IDX pu_idx;
    PU&    pu = New_PU (pu_idx);

    PU_Init (pu, fioruntime_ty, CURRENT_SYMTAB);

    ST_Init ( st, Save_Str ( impld_name ), CLASS_FUNC, SCLASS_EXTERN,
              EXPORT_PREEMPTIBLE, (TY_IDX) pu_idx);
    WN_st_idx (wn) =  ST_st_idx (st);
    impld_fio_sts[form] = st;
  } else
    WN_st_idx (wn) = ST_st_idx (impld_fio_sts[form]);

  WN_Set_Call_Non_Data_Mod ( wn );
  WN_Set_Call_Non_Data_Ref ( wn );
  WN_Set_Call_Parm_Mod ( wn );
  WN_Set_Call_Parm_Ref ( wn );

  /*  Fill in any or all function arguments to the call.  */

  nkids = 0;
  switch (form) {
    case FFT_FORMAT   :
    case FFT_LIST     :
      /* For Formatted & List-directed, first argument is the element type */
      tmp_st = Gen_Temp_Symbol( Be_Type_Tbl(MTYPE_I4), "io_item_type" );
      WN_INSERT_BlockLast ( block, WN_CreateStid( OPC_I4STID, 0, tmp_st,
			Be_Type_Tbl(MTYPE_I4), 
			WN_CreateIntconst( OPC_I4INTCONST, fio_types[type] )));
      WN_kid(wn, nkids++) = Gen_Parm_WN ( Make_IoAddr_WN( tmp_st ) );
    case FFT_UNFORMAT :

      /* Second/First argument is the array element */
      WN_kid(wn, nkids++) = Gen_Parm_WN ( arr_item );

      /* Third/Second argument is the Implied-do variables */
      dovar = WN_COPY_Tree(WN_index (impld_item));
      WN_kid(wn, nkids++) = Gen_Parm_WN( Make_IoAddr_WN( WN_st( dovar ) ) );

      /* Fourth/third argument is the lower bound */
      tmp_st = Gen_Temp_Symbol( Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), "lower_bound" );
      WN_INSERT_BlockLast ( block, WN_CreateStid( 
		(Pointer_Size == 4) ? OPC_I4STID : OPC_I8STID, 0, tmp_st,
				Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), 
				WN_COPY_Tree(WN_start(impld_item))));
      WN_kid(wn, nkids++) = Gen_Parm_WN( Make_IoAddr_WN( tmp_st ) );

      /* Fifth/Fourth argument is the upper bound */
      tmp_st = Gen_Temp_Symbol( Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), "upper_bound" );
      WN_INSERT_BlockLast ( block, WN_CreateStid( 
		(Pointer_Size == 4) ? OPC_I4STID : OPC_I8STID, 0, tmp_st,
				Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), 
				WN_COPY_Tree(WN_end(impld_item))));
      WN_kid(wn, nkids++) = Gen_Parm_WN( Make_IoAddr_WN( tmp_st ) );

      /* Sixth/Fifth argument is the step */
      tmp_st = Gen_Temp_Symbol( Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), "do_step" );
      WN_INSERT_BlockLast ( block, WN_CreateStid( 
		(Pointer_Size == 4) ? OPC_I4STID : OPC_I8STID, 0, tmp_st,
				Be_Type_Tbl(
		(Pointer_Size == 4) ? MTYPE_I4 : MTYPE_I8), 
				WN_COPY_Tree(WN_step(impld_item))));
      WN_kid(wn, nkids++) = Gen_Parm_WN( Make_IoAddr_WN( tmp_st ) );

      /* Seventh/Sixth argument is the mp_unit_ptr if needed */
      if (mp_unit_ptr) {
	WN_kid(wn, nkids++) = Gen_Parm_WN( mp_unit_ptr );
      }

      /* Eighth/Seventh argument is the size of the array element, 
	 passed by value */
      aty = TY_pointed( WN_ty( WN_kid0(arr_item) ) );
      size = TY_size( TY_AR_etype(aty) );
      WN_kid(wn, nkids++) = Gen_Parm_WN( 
			WN_CreateIntconst ( OPC_I4INTCONST, size ) );

      /* Ninth/Eighth argument is the size of the implied-do variable, 
	passed by value */
      size = TY_size( Ty_Table[ ST_type( WN_st( dovar) ) ]);
      WN_kid(wn, nkids++) = Gen_Parm_WN( 
			WN_CreateIntconst ( OPC_I4INTCONST, size ) );
      break;
    default:
      Fail_FmtAssertion("Gen_Impld_Io_Calls: illegal format type (%d)"
			" in I/O processing", form);
  }

  /*  Insert the call into the statement block being created for the I/O  */
  /*  statement.							  */

  WN_INSERT_BlockLast ( block, wn );

  /*  Add any statements needed to handle the status result.  */

 if (iostat1 != NULL)
   WN_INSERT_BlockLast ( block, WN_COPY_Tree ( iostat1 ) );
}

/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take an      */
/*  integer constant value and put it at the specified offset within the
    structure */

static void Gen_Iolist_PutFieldConst ( WN * block, ST * st, INT32 foffset, 
                                       INT32 ftype, INT64 value )
{
  WN * wn = NULL;

  /*  Generate an appropriate INTCONST node and store it into the structure  */
  /*  field.                                                                 */

  if (ftype == MTYPE_I4)
    wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
         WN_CreateIntconst ( OPC_I4INTCONST, value ));
  else if (ftype == MTYPE_U4)
    wn = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
         WN_CreateIntconst ( OPC_U4INTCONST, value ));
  else if (ftype == MTYPE_I8)
    wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
         WN_CreateIntconst ( OPC_I8INTCONST, value ));
  else if (ftype == MTYPE_U8)
    wn = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
         WN_CreateIntconst ( OPC_U8INTCONST, value ));
  else
    Fail_FmtAssertion("Gen_Iolist_PutFieldConst: unexpected field type (%s) "
		      "in I/O processing", MTYPE_name(ftype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.                                               */

  WN_INSERT_BlockLast ( block, wn );

}

/*  The mload/mstore generates WHIRL based on the TY_align, which must     */
/*  be correct. Create a new TY based on the alignment and offset	   */

static TY_IDX Create_Maligned_TY(INT32 offset, TY_IDX ty) 
{
  INT32 align=	compute_offset_alignment(offset, TY_align(ty));

  if (align) {
    TY_IDX newTY;
    newTY = Make_Align_Type(MTYPE_To_TY(MTYPE_M), align);

    return Make_Pointer_Type(newTY, FALSE);
  }
  ty = Make_Align_Type (MTYPE_To_TY (MTYPE_M), TY_align(ty));
  return TY_pointer(ty);
}

/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take the     */
/*  value of a user expression specified by a WN and put it at the          */
/*  specified offset within the I/O structures.                             */

static void Gen_Iolist_PutFieldWN ( WN * block, ST * st, INT32 foffset, 
                                                         INT32 ftype, WN * wn ) 
{
  WN * wnx = NULL;
  INT32 etype;
  INT32 vtype;

  /*  Determine kind and type of expression node.  Then convert the node as  */
  /*  appropriate to match the structure field type.  Finally, generate the  */
  /*  store into the field.						     */
  /*									     */
  /*  The three cases handled are:					     */
  /*	1) OPC_I4INTCONST or OPC_I8INTCONST - this will be used directly     */
  /*					      or have it's size adjusted.    */
  /*	2) any expression tree - this will be used directly with the	     */
  /*				 addition of a CVT node if needed.	     */

  if (WN_opcode(wn) == OPC_I4INTCONST)

    if (ftype == MTYPE_I4)
      wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
			    wn );
    else if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4), 
	    WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_I8) {
      wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	    WN_CreateIntconst ( OPC_I8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
	    WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, I4INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_I8INTCONST)

    if (ftype == MTYPE_I4) {
      wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	    WN_CreateIntconst ( OPC_I4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	    WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
	    WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_I8)
      wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			    wn );
    else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, I8INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U4INTCONST)

    if (ftype == MTYPE_U4)
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
			    wn );
    else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
	    WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, U4INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U8INTCONST)

    if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	    WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8)
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
			    wn );
    else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, U8INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U4LDA || WN_opcode(wn) == OPC_U8LDA) {

    vtype = ST_btype(WN_st(wn));
    if (ftype == MTYPE_I4)
      if (vtype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateLdid ( OPC_I4I4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I4) ));
      else if (vtype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4I8CVT,
	      WN_CreateLdid ( OPC_I8I8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) )));
      else if (vtype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F4CVT,
	      WN_CreateLdid ( OPC_F4F4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F4) )));
      else if (vtype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F8CVT,
	      WN_CreateLdid ( OPC_F8F8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F8) )));
#if defined(TARG_IA64) || defined(TARG_X8664)
      else if (vtype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4F10CVT,
              WN_CreateLdid ( OPC_F10F10LDID, WN_offset(wn), WN_st(wn),
                              Be_Type_Tbl(MTYPE_F10) )));
#endif
      else if (vtype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4FQCVT,
	      WN_CreateLdid ( OPC_FQFQLDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_FQ) )));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, LDA,I4: unexpected item"
			  " type (%s) in I/O processing", MTYPE_name(vtype));
    else if (ftype == MTYPE_I8)
      if (vtype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateLdid ( OPC_I8I4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) ));
      else if (vtype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateLdid ( OPC_I8I8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) ));
      else if (vtype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F4CVT,
	      WN_CreateLdid ( OPC_F4F4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F4) )));
      else if (vtype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F8CVT,
	      WN_CreateLdid ( OPC_F8F8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F8) )));
#if defined(TARG_IA64) || defined(TARG_X8664)
      else if (vtype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8F10CVT,
              WN_CreateLdid ( OPC_F10F10LDID, WN_offset(wn), WN_st(wn),
                              Be_Type_Tbl(MTYPE_F10) )));
#endif
      else if (vtype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8FQCVT,
	      WN_CreateLdid ( OPC_FQFQLDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_FQ) )));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, LDA,I8: unexpected item"
			  " type (%s) in I/O processing", MTYPE_name(vtype));
    else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, U4LDA/U8LDA: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));
    WN_Delete ( wn );

  } else if (OPCODE_is_expression(WN_opcode(wn))) {

    etype = WN_rtype(wn);
    if (ftype == MTYPE_I4)
      if (etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
			      wn );
      else if (etype == MTYPE_U4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      		      wn );
      else if (etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4I8CVT, wn ));
      else if (etype == MTYPE_U8)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4U8CVT, wn ));
      else if (etype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F4CVT, wn ));
      else if (etype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F8CVT, wn ));
      else if (etype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4F10CVT, wn ));
      else if (etype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4FQCVT, wn ));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, exp,I4: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_U4)
      if (etype == MTYPE_U4 || etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	      		      wn );
      else if (etype == MTYPE_U8)
        wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
              WN_CreateExp1 ( OPC_U4U8CVT, wn ));
      else if (etype == MTYPE_I8)
        wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
              WN_CreateExp1 ( OPC_U4I8CVT, wn ));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, exp,U4: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_I8)
      if (etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8I4CVT, wn ));
      else if (etype == MTYPE_U4)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8U4CVT, wn ));
      else if (etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			      wn );
      else if (etype == MTYPE_U8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			      wn );
      else if (etype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F4CVT, wn ));
      else if (etype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F8CVT, wn ));
      else if (etype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8F10CVT, wn ));
      else if (etype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8FQCVT, wn ));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, exp,I8: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_U8)
      if (etype == MTYPE_U8 || etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
			      wn );
      else if (etype == MTYPE_U4)
        wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
              WN_CreateExp1 ( OPC_U8U4CVT, wn ));
      else if (etype == MTYPE_I4)
        wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
              WN_CreateExp1 ( OPC_U8I4CVT, wn ));
      else
	Fail_FmtAssertion("Gen_Iolist_PutFieldWN, exp,U8: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else
      Fail_FmtAssertion("Gen_Iolist_PutFieldWN, expression: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  } else

    Fail_FmtAssertion("Gen_Iolist_PutFieldWN: unexpected WN_opcode (%s) in"
		      " I/O processing", OPCODE_name(WN_opcode(wn)));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wnx );

}

/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take an      */
/*  address (given by an LDA node, ARRAY node or an LDID node) and put it   */
/*  at the specified offset within the structure */

static void Gen_Iolist_PutAddrWN ( WN * block, ST * st, INT32 foffset,
				   INT32 ftype, WN * wn )
{
  WN * wnx;

  /*  Save the ST info for building dummy parm lists.  */

#ifdef KEY
  // Bug 10413: IPA may have constant propagated a non-existing optional
  // argument to an IO_ITEM, skip it.
  if (WN_operator(wn) == OPR_INTCONST)
  {
    // The constant must be zero.
    FmtAssert (WN_const_val(wn) == 0,
               ("Get_Iolist_PutAddrWN: INTCONST can only be zero"));
    return;
  }
#endif

  wnx = wn;
  Add_To_Dummy_List(wnx);

  /*  Generate the appropriate STID depending on the field type and  */
  /*  child node.						     */

  if ((WN_opcode(wn) == OPC_U4LDA   || WN_opcode(wn) == OPC_U4U4LDID  ||
       WN_opcode(wn) == OPC_U4ARRAY || WN_opcode(wn) == OPC_U4U4ILOAD ||
       WN_opcode(wn) == OPC_U4ADD || WN_opcode(wn) == OPC_I4I4LDID)  &&
      (ftype == MTYPE_U4 || ftype == MTYPE_I4 || ftype == MTYPE_M ))
    wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4), wn );

  else if ((WN_opcode(wn) == OPC_U4LDA   || WN_opcode(wn) == OPC_U4U4LDID  ||
       WN_opcode(wn) == OPC_U4ARRAY || WN_opcode(wn) == OPC_U4U4ILOAD ||
       WN_opcode(wn) == OPC_U4ADD)  &&
      (ftype == MTYPE_U8 || ftype == MTYPE_I8 ))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), 
          WN_CreateExp1 ( OPC_U8U4CVT, wn));

  else if ((WN_opcode(wn) == OPC_U8LDA   || WN_opcode(wn) == OPC_U8U8LDID  ||
	    WN_opcode(wn) == OPC_U8ARRAY || WN_opcode(wn) == OPC_U8U8ILOAD ||
	    WN_opcode(wn) == OPC_U8ADD || WN_opcode(wn) == OPC_I8I8LDID)  &&
	   (ftype == MTYPE_U8 || ftype == MTYPE_I8 || ftype == MTYPE_M))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), wn );

  else if ((WN_opcode(wn) == OPC_U8LDA   || WN_opcode(wn) == OPC_U8U8LDID  ||
	    WN_opcode(wn) == OPC_U8ARRAY || WN_opcode(wn) == OPC_U8U8ILOAD ||
	    WN_opcode(wn) == OPC_U8ADD)  &&
	   (ftype == MTYPE_U4 || ftype == MTYPE_I4 ))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), 
          WN_CreateExp1 ( OPC_U4U8CVT, wn ));

  else if (WN_opcode(wn) == OPC_MLOAD) 
    wnx = WN_CreateMstore(foffset, Create_Maligned_TY(foffset, ST_type(st)), wn,
	  WN_CreateLda(opc_lda, 0, TY_pointer(ST_type(st)), st),
	  WN_CreateIntconst ( OPC_U4INTCONST, fcd_size));
  else 
    Fail_FmtAssertion("Gen_Iolist_PutAddrWN: unexpected WN_opcode (%s) and"
		      " field type (%s) in I/O processing",
		      OPCODE_name(WN_opcode(wn)), MTYPE_name(ftype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wnx );

}

/*  This routine is one of a set to manage the structures passed between     */
/*  user code and the I/O runtime routines.  This routine will get any       */
/*  field within any of the I/O structures and store the value into a        */
/*  user variable specified by an ST entry.  All necessary data conversions  */
/*  are generated.							     */

static void Gen_Io_GetFieldST ( WN * block, ST * var, ST * st, FIOSTRUCT field )
{
  WN * wn;
  INT32 foffset;
  INT32 ftype;
  INT32 vtype = ST_btype(var);

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Based on the field type and the user variable type, generate the  */
  /*  ldid/stid pair plus any additional conversion needed.		*/

  if (vtype == MTYPE_I4)

    if (ftype == MTYPE_I4)
      wn = WN_CreateStid ( OPC_I4STID, 0, var, ST_type(var),
	   WN_CreateLdid ( OPC_I4I4LDID, foffset, st, Be_Type_Tbl(MTYPE_I4) ));
    else if (ftype == MTYPE_I8)
      wn = WN_CreateStid ( OPC_I4STID, 0, var, ST_type(var),
	   WN_CreateExp1 ( OPC_I4I8CVT,
	   WN_CreateLdid ( OPC_I8I8LDID, foffset, st, Be_Type_Tbl(MTYPE_I8) )));
    else
      Fail_FmtAssertion("Gen_Io_GetFieldST, I4: unexpected field type (%s)"
			" in I/O processing", MTYPE_name(ftype));

  else if (vtype == MTYPE_I8)

    if (ftype == MTYPE_I4)
      wn = WN_CreateStid ( OPC_I8STID, 0, var, ST_type(var),
	   WN_CreateLdid ( OPC_I8I4LDID, foffset, st, Be_Type_Tbl(MTYPE_I8) ));
    else if (ftype == MTYPE_I8)
      wn = WN_CreateStid ( OPC_I8STID, 0, var, ST_type(var),
	   WN_CreateLdid ( OPC_I8I8LDID, foffset, st, Be_Type_Tbl(MTYPE_I8) ));
    else
      Fail_FmtAssertion("Gen_Io_GetFieldST, I8: unexpected field type (%s)"
			" in I/O processing", MTYPE_name(ftype));

  else

    Fail_FmtAssertion("Gen_Io_GetFieldST: unexpected item type (%s) in"
		      " I/O processing", MTYPE_name(vtype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wn );

}


/*  This routine is one of a set to manage the structures passed between     */
/*  user code and the I/O runtime routines.  This routine will get any       */
/*  field within any of the I/O structures and store the value into a        */
/*  user variable specified by an WN node.  All necessary data conversions   */
/*  are generated.							     */

static void Gen_Io_GetFieldWN ( WN * block, WN * wn, ST * st, FIOSTRUCT field )
{
  WN * wnx;
  INT32 foffset;
  INT32 ftype;
  ST * var = WN_st(wn);
  INT32 vtype = ST_btype(var);

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Based on the field type and the user variable type, generate the  */
  /*  ldid/stid pair plus any additional conversion needed.		*/

  if (vtype == MTYPE_I4)

    if (ftype == MTYPE_I4)
      wnx = WN_CreateStid ( OPC_I4STID, 0, var, ST_type(var),
	    WN_CreateLdid ( OPC_I4I4LDID, foffset, st, Be_Type_Tbl(MTYPE_I4) ));
    else if (ftype == MTYPE_I8)
      wnx = WN_CreateStid ( OPC_I4STID, 0, var, ST_type(var),
	    WN_CreateExp1 ( OPC_I4I8CVT,
	    WN_CreateLdid ( OPC_I8I8LDID, foffset, st,
			    Be_Type_Tbl(MTYPE_I8) )));
    else
      Fail_FmtAssertion("Gen_Io_GetFieldWN, I4: unexpected field type (%s)"
			" in I/O processing", MTYPE_name(ftype));

  else if (vtype == MTYPE_I8)

    if (ftype == MTYPE_I4)
      wnx = WN_CreateStid ( OPC_I8STID, 0, var, ST_type(var),
	    WN_CreateLdid ( OPC_I8I4LDID, foffset, st, Be_Type_Tbl(MTYPE_I8) ));
    else if (ftype == MTYPE_I8)
      wnx = WN_CreateStid ( OPC_I8STID, 0, var, ST_type(var),
	    WN_CreateLdid ( OPC_I8I8LDID, foffset, st, Be_Type_Tbl(MTYPE_I8) ));
    else
      Fail_FmtAssertion("Gen_Io_GetFieldWN, I8: unexpected field type (%s)"
			" in I/O processing", MTYPE_name(ftype));

  else

    Fail_FmtAssertion("Gen_Io_GetFieldWN: unexpected item type (%s)"
		      " in I/O processing", MTYPE_name(vtype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wnx );

  /*  Delete the old load node.  */

  WN_Delete ( wn );

}


/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take the     */
/*  value of a user variable specified by an ST and put it into any field   */
/*  within any of the I/O structures.  All necessary data conversions are   */
/*  generated.								    */

static void Gen_Io_PutFieldST ( WN * block, ST * st, FIOSTRUCT field, ST * var )
{
  WN * wn;
  INT32 foffset;
  INT32 ftype;
  INT32 vtype = ST_btype(var);

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Based on the field type and the user variable type, generate the  */
  /*  ldid/stid pair plus any additional conversion needed.		*/

  if (ftype == MTYPE_I4)

    if (vtype == MTYPE_I4)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	   WN_CreateLdid ( OPC_I4I4LDID, 0, var, Be_Type_Tbl(MTYPE_I4) ));
    else if (vtype == MTYPE_I8)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	   WN_CreateExp1 ( OPC_I4I8CVT,
	   WN_CreateLdid ( OPC_I8I8LDID, 0, var, Be_Type_Tbl(MTYPE_I8) )));
    else if (vtype == MTYPE_F4)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	   WN_CreateExp1 ( OPC_I4F4CVT,
	   WN_CreateLdid ( OPC_F4F4LDID, 0, var, Be_Type_Tbl(MTYPE_F4) )));
    else if (vtype == MTYPE_F8)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	   WN_CreateExp1 ( OPC_I4F8CVT,
	   WN_CreateLdid ( OPC_F8F8LDID, 0, var, Be_Type_Tbl(MTYPE_F8) )));
    else if (vtype == MTYPE_F10)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
           WN_CreateExp1 ( OPC_I4F10CVT,
           WN_CreateLdid ( OPC_F10F10LDID, 0, var, Be_Type_Tbl(MTYPE_F10) )));
    else if (vtype == MTYPE_FQ)
      wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	   WN_CreateExp1 ( OPC_I4FQCVT,
	   WN_CreateLdid ( OPC_FQFQLDID, 0, var, Be_Type_Tbl(MTYPE_FQ) )));
    else
      Fail_FmtAssertion("Gen_Io_PutFieldST, I4: unexpected item type (%s)"
			" in I/O processing", MTYPE_name(vtype));

  else if (ftype == MTYPE_I8)

    if (vtype == MTYPE_I4)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	   WN_CreateLdid ( OPC_I8I4LDID, 0, var, Be_Type_Tbl(MTYPE_I8) ));
    else if (vtype == MTYPE_I8)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	   WN_CreateLdid ( OPC_I8I8LDID, 0, var, Be_Type_Tbl(MTYPE_I8) ));
    else if (vtype == MTYPE_F4)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	   WN_CreateExp1 ( OPC_I8F4CVT,
	   WN_CreateLdid ( OPC_F4F4LDID, 0, var, Be_Type_Tbl(MTYPE_F4) )));
    else if (vtype == MTYPE_F8)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	   WN_CreateExp1 ( OPC_I8F8CVT,
	   WN_CreateLdid ( OPC_F8F8LDID, 0, var, Be_Type_Tbl(MTYPE_F8) )));
    else if (vtype == MTYPE_F10)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
           WN_CreateExp1 ( OPC_I8F10CVT,
           WN_CreateLdid ( OPC_F10F10LDID, 0, var, Be_Type_Tbl(MTYPE_F10) )));
    else if (vtype == MTYPE_FQ)
      wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	   WN_CreateExp1 ( OPC_I8FQCVT,
	   WN_CreateLdid ( OPC_FQFQLDID, 0, var, Be_Type_Tbl(MTYPE_FQ) )));
    else
      Fail_FmtAssertion("Gen_Io_PutFieldST, I8: unexpected item type (%s)"
			" in I/O processing", MTYPE_name(vtype));

  else

    Fail_FmtAssertion("Gen_Io_PutFieldST: unexpected field type (%s)"
		      " in I/O processing", MTYPE_name(ftype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wn );

}


/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take the     */
/*  value of a user expression specified by a WN and put it into any field  */
/*  within any of the I/O structures.  All necessary data conversions are   */
/*  generated.								    */

static void Gen_Io_PutFieldWN ( WN * block, ST * st, FIOSTRUCT field, WN * wn )
{
  WN * wnx = NULL;
  INT32 foffset;
  INT32 ftype;
  INT32 etype;
  INT32 vtype;

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Determine kind and type of expression node.  Then convert the node as  */
  /*  appropriate to match the structure field type.  Finally, generate the  */
  /*  store into the field.						     */
  /*									     */
  /*  The three cases handled are:					     */
  /*	1) OPC_I4INTCONST or OPC_I8INTCONST - this will be used directly     */
  /*					      or have it's size adjusted.    */
  /*	2) OPC_U4LDA or OPC_U8LDA - this will be replaced with an LDID node  */
  /*				    with the same ST entry and possibly a    */
  /*				    CVT node.				     */
  /*	3) any expression tree - this will be used directly with the	     */
  /*				 addition of a CVT node if needed.	     */

  if (WN_opcode(wn) == OPC_I4INTCONST)

    if (ftype == MTYPE_I4)
      wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
			    wn );
    else if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
            WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_I8) {
      wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
            WN_CreateIntconst ( OPC_I8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
            WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, I4INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_I8INTCONST)

    if (ftype == MTYPE_I4) {
      wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
            WN_CreateIntconst ( OPC_I4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
            WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
            WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_I8)
      wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			    wn );
    else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, I8INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U4INTCONST)

    if (ftype == MTYPE_U4)
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
			    wn );
    else if (ftype == MTYPE_U8) {
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
	    WN_CreateIntconst ( OPC_U8INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, U4INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U8INTCONST)

    if (ftype == MTYPE_U4) {
      wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	    WN_CreateIntconst ( OPC_U4INTCONST, WN_const_val(wn) ));
      WN_Delete ( wn );
    } else if (ftype == MTYPE_U8)
      wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
			    wn );
    else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, U8INTCONST: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  else if (WN_opcode(wn) == OPC_U4LDA || WN_opcode(wn) == OPC_U8LDA) {

    vtype = ST_btype(WN_st(wn));
    if (ftype == MTYPE_I4)
      if (vtype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateLdid ( OPC_I4I4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I4) ));
      else if (vtype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4I8CVT,
	      WN_CreateLdid ( OPC_I8I8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) )));
      else if (vtype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F4CVT,
	      WN_CreateLdid ( OPC_F4F4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F4) )));
      else if (vtype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F8CVT,
	      WN_CreateLdid ( OPC_F8F8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F8) )));
      else if (vtype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4F10CVT,
              WN_CreateLdid ( OPC_F10F10LDID, WN_offset(wn), WN_st(wn),
                              Be_Type_Tbl(MTYPE_F10) )));
      else if (vtype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4FQCVT,
	      WN_CreateLdid ( OPC_FQFQLDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_FQ) )));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, LDA,I4: unexpected item type"
			  " (%s) in I/O processing", MTYPE_name(vtype));
    else if (ftype == MTYPE_I8)
      if (vtype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateLdid ( OPC_I8I4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) ));
      else if (vtype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateLdid ( OPC_I8I8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_I8) ));
      else if (vtype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F4CVT,
	      WN_CreateLdid ( OPC_F4F4LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F4) )));
      else if (vtype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F8CVT,
	      WN_CreateLdid ( OPC_F8F8LDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_F8) )));
      else if (vtype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8F10CVT,
              WN_CreateLdid ( OPC_F10F10LDID, WN_offset(wn), WN_st(wn),
                              Be_Type_Tbl(MTYPE_F10) )));
      else if (vtype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8FQCVT,
	      WN_CreateLdid ( OPC_FQFQLDID, WN_offset(wn), WN_st(wn),
			      Be_Type_Tbl(MTYPE_FQ) )));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, LDA,I8: unexpected item type"
			  " (%s) in I/O processing", MTYPE_name(vtype));
    else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, U4LDA/U8LDA: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));
    WN_Delete ( wn );

  } else if (OPCODE_is_expression(WN_opcode(wn))) {

    etype = WN_rtype(wn);
    if (ftype == MTYPE_I4)
      if (etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
			      wn );
      else if (etype == MTYPE_U4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      		      wn );
      else if (etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4I8CVT, wn ));
      else if (etype == MTYPE_U8)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4U8CVT, wn ));
      else if (etype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F4CVT, wn ));
      else if (etype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4F8CVT, wn ));
      else if (etype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
              WN_CreateExp1 ( OPC_I4F10CVT, wn ));
      else if (etype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	      WN_CreateExp1 ( OPC_I4FQCVT, wn ));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, exp,I4: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_U4)
      if (etype == MTYPE_U4 || etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	      		      wn );
      else if (etype == MTYPE_U8)
        wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
              WN_CreateExp1 ( OPC_U4U8CVT, wn ));
      else if (etype == MTYPE_I8)
        wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
              WN_CreateExp1 ( OPC_U4I8CVT, wn ));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, exp,U4: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_I8)
      if (etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8I4CVT, wn ));
      else if (etype == MTYPE_U4)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8U4CVT, wn ));
      else if (etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			      wn );
      else if (etype == MTYPE_U8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
			      wn );
      else if (etype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F4CVT, wn ));
      else if (etype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8F8CVT, wn ));
      else if (etype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
              WN_CreateExp1 ( OPC_I8F10CVT, wn ));
      else if (etype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	      WN_CreateExp1 ( OPC_I8FQCVT, wn ));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, exp,I8: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else if (ftype == MTYPE_U8)
      if (etype == MTYPE_U8 || etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
			      wn );
      else if (etype == MTYPE_U4)
        wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
              WN_CreateExp1 ( OPC_U8U4CVT, wn ));
      else if (etype == MTYPE_I4)
        wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
              WN_CreateExp1 ( OPC_U8I4CVT, wn ));
      else
	Fail_FmtAssertion("Gen_Io_PutFieldWN, exp,U8: unexpected expression"
			  " type (%s) in I/O processing", MTYPE_name(etype));
    else
      Fail_FmtAssertion("Gen_Io_PutFieldWN, expression: unexpected field"
			" type (%s) in I/O processing", MTYPE_name(ftype));

  } else

    Fail_FmtAssertion("Gen_Io_PutFieldWN: unexpected node (%s)"
		      " in I/O processing", OPCODE_name(WN_opcode(wn)));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wnx );

}


/*  This routine is used to set the Keyspec structure.  This routine will   */
/*  take an array of values specified by a WN and put it into an array of   */
/*  structures specified by the ST. All necessary data conversions are      */
/*  generated.								    */

static void Gen_Io_PutKeyFieldWN ( WN * block, ST * st, WN ** wn, INT32 nkeys )
{
  WN * wnx = NULL;
  INT32 foffset;
  INT32 etype;
  INT32 vtype;
  INT32 i;

  /*  Lookup the offset and type of the requested field.  */

  foffset = 0;

  /*  Go through the list of all nkeys and assign the WN values into the     */
  /*  array of Keyspec structure                                             */

  for (i = 0; i < nkeys; i++, wn++, foffset += 2) {
  /*  Determine kind and type of expression node.  Then convert the node to  */
  /*  INT16 as needed.  Finally, store into the field.                       */

    if (WN_opcode(*wn) == OPC_I4INTCONST) {

      wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	    WN_CreateIntconst ( OPC_I4INTCONST, WN_const_val(*wn) ));
      WN_Delete ( *wn );

    } else if (WN_opcode(*wn) == OPC_I8INTCONST) {

      wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateIntconst ( OPC_I4INTCONST, WN_const_val(*wn) ));
      WN_Delete ( *wn );

    } else if (WN_opcode(*wn) == OPC_U4LDA || WN_opcode(*wn) == OPC_U8LDA) {

      vtype = ST_btype(WN_st(*wn));
      if (vtype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateLdid ( OPC_I4I4LDID, WN_offset(*wn), WN_st(*wn),
			      Be_Type_Tbl(MTYPE_I4) ));
      else if (vtype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4I8CVT,
	      WN_CreateLdid ( OPC_I8I8LDID, WN_offset(*wn), WN_st(*wn),
			      Be_Type_Tbl(MTYPE_I8) )));
      else if (vtype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4F4CVT,
	      WN_CreateLdid ( OPC_F4F4LDID, WN_offset(*wn), WN_st(*wn),
			      Be_Type_Tbl(MTYPE_F4) )));
      else if (vtype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4F8CVT,
	      WN_CreateLdid ( OPC_F8F8LDID, WN_offset(*wn), WN_st(*wn),
			      Be_Type_Tbl(MTYPE_F8) )));
      else if (vtype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
              WN_CreateExp1 ( OPC_I4F10CVT,
              WN_CreateLdid ( OPC_F10F10LDID, WN_offset(*wn), WN_st(*wn),
                              Be_Type_Tbl(MTYPE_F10) )));
      else if (vtype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I2STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4FQCVT,
	      WN_CreateLdid ( OPC_FQFQLDID, WN_offset(*wn), WN_st(*wn),
			      Be_Type_Tbl(MTYPE_FQ) )));
      else
	Fail_FmtAssertion("Gen_Io_PutKeyFieldWN: unexpected item type (%s)"
			  " in I/O processing", MTYPE_name(vtype));
      WN_Delete ( *wn );

    } else if (OPCODE_is_expression(WN_opcode(*wn))) {

      etype = WN_rtype(*wn);
      if (etype == MTYPE_I4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      *wn );
      else if (etype == MTYPE_I8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4I8CVT, *wn ));
      else if (etype == MTYPE_F4)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4F4CVT, *wn ));
      else if (etype == MTYPE_F8)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4F8CVT, *wn ));
      else if (etype == MTYPE_F10)
        wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
              WN_CreateExp1 ( OPC_I4F10CVT, *wn ));
      else if (etype == MTYPE_FQ)
	wnx = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I2),
	      WN_CreateExp1 ( OPC_I4FQCVT, *wn ));
      else
	Fail_FmtAssertion("Gen_Io_PutKeyFieldWN: unexpected expression type"
			  " (%s) in I/O processing", MTYPE_name(etype));
    } else
      Fail_FmtAssertion("Gen_Io_PutKeyFieldWN: unexpected node (%s)"
			" in I/O processing", OPCODE_name(WN_opcode(*wn)));

    /*  Add the generated load/store into the statement block being created  */
    /*  for the I/O statement.						   */

    WN_INSERT_BlockLast ( block, wnx );
  }
}


/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take an      */
/*  integer constant value and put it into any field within any of the      */
/*  seven structures.  The appropriate INTCONST node is generated based on  */
/*  the field type.							    */

static void Gen_Io_PutFieldConst ( WN * block, ST * st, FIOSTRUCT field,
				   INT64 value )
{
  WN * wn = NULL;
  INT32 foffset;
  INT32 ftype;

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Generate an appropriate INTCONST node and store it into the structure  */
  /*  field.								     */

  if (ftype == MTYPE_I4)
    wn = WN_CreateStid ( OPC_I4STID, foffset, st, Be_Type_Tbl(MTYPE_I4),
	 WN_CreateIntconst ( OPC_I4INTCONST, value ));
  else if (ftype == MTYPE_U4)
    wn = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4),
	 WN_CreateIntconst ( OPC_U4INTCONST, value ));
  else if (ftype == MTYPE_I8)
    wn = WN_CreateStid ( OPC_I8STID, foffset, st, Be_Type_Tbl(MTYPE_I8),
	 WN_CreateIntconst ( OPC_I8INTCONST, value ));
  else if (ftype == MTYPE_U8)
    wn = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8),
	 WN_CreateIntconst ( OPC_U8INTCONST, value ));
  else
    Fail_FmtAssertion("Gen_Io_PutFieldConst: unexpected field type (%s)"
		      " in I/O processing", MTYPE_name(ftype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wn );

}


/*  This routine is one of a set to manage the structures passed between    */
/*  user code and the I/O runtime routines.  This routine will take an      */
/*  address (given by an LDA node, ARRAY node or an LDID node) and put it   */
/*  into any field within any of the I/O structures.  It will also save the */
/*  ST info for use by Gen_Io_Calls to build a dummy argument list of       */
/*  referenced variables.						    */

static void Gen_Io_PutAddrWN ( WN * block, ST * st, FIOSTRUCT field, WN * wn )
{
  INT32 foffset;
  INT32 ftype;
  WN * wnx;

#ifdef KEY
  // Bug 10413: IPA may have constant propagated a non-existing optional
  // argument to an IO_ITEM, skip it.
  if (WN_operator(wn) == OPR_INTCONST)
  {
    FmtAssert (WN_const_val(wn) == 0,
               ("Gen_Io_PutAddrWN: INTCONST can only be zero"));
    return;
  }
#endif

  /*  Save the ST info for building dummy parm lists.  */

  wnx = wn;
  Add_To_Dummy_List(wnx);

  /*  Lookup the offset and type of the requested field.  */

  if (Pointer_Size == 4) {
    foffset = fiostruct_info[field].offset32;
    ftype = fiostruct_info[field].type32;
  } else {
    foffset = fiostruct_info[field].offset64;
    ftype = fiostruct_info[field].type64;
  }

  /*  Generate the appropriate STID depending on the field type and  */
  /*  child node.						     */

  if ((WN_opcode(wn) == OPC_U4LDA   || WN_opcode(wn) == OPC_U4U4LDID  ||
       WN_opcode(wn) == OPC_U4ARRAY || WN_opcode(wn) == OPC_U4U4ILOAD ||
       WN_opcode(wn) == OPC_U4ADD || WN_opcode(wn) == OPC_I4I4LDID)  &&
      (ftype == MTYPE_U4 || ftype == MTYPE_I4 || ftype == MTYPE_M))
    wnx = WN_CreateStid ( OPC_U4STID, foffset, st, Be_Type_Tbl(MTYPE_U4), wn );

  else if ((WN_opcode(wn) == OPC_U4LDA   || WN_opcode(wn) == OPC_U4U4LDID  ||
       WN_opcode(wn) == OPC_U4ARRAY || WN_opcode(wn) == OPC_U4U4ILOAD ||
       WN_opcode(wn) == OPC_U4ADD)  &&
      (ftype == MTYPE_U8 || ftype == MTYPE_I8 ))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), 
          WN_CreateExp1 ( OPC_U8U4CVT, wn));

  else if ((WN_opcode(wn) == OPC_U8LDA   || WN_opcode(wn) == OPC_U8U8LDID  ||
	    WN_opcode(wn) == OPC_U8ARRAY || WN_opcode(wn) == OPC_U8U8ILOAD ||
	    WN_opcode(wn) == OPC_U8ADD || WN_opcode(wn) == OPC_I8I8LDID)  &&
	   (ftype == MTYPE_U8 || ftype == MTYPE_I8 || ftype == MTYPE_M))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), wn );

  else if ((WN_opcode(wn) == OPC_U8LDA   || WN_opcode(wn) == OPC_U8U8LDID  ||
	    WN_opcode(wn) == OPC_U8ARRAY || WN_opcode(wn) == OPC_U8U8ILOAD ||
	    WN_opcode(wn) == OPC_U8ADD)   &&
	   (ftype == MTYPE_U4 || ftype == MTYPE_I4 ))
    wnx = WN_CreateStid ( OPC_U8STID, foffset, st, Be_Type_Tbl(MTYPE_U8), 
          WN_CreateExp1 ( OPC_U4U8CVT, wn ));

  else if (WN_opcode(wn) == OPC_MLOAD) 
    wnx = WN_CreateMstore(foffset, Create_Maligned_TY(foffset, ST_type(st)), wn,
	  WN_CreateLda(opc_lda, 0, TY_pointer(ST_type(st)), st),
	  WN_CreateIntconst ( OPC_U4INTCONST, fcd_size));
  else 
    Fail_FmtAssertion("Gen_Io_PutAddrWN: unexpected field type (%s)"
		      " in I/O processing", MTYPE_name(ftype));

  /*  Add the generated load/store into the statement block being created  */
  /*  for the I/O statement.						   */

  WN_INSERT_BlockLast ( block, wnx );

}


static void
Set_Cilist_Fields( WN * block, ST *st,
		   WN *unit_wn, WN **items, WN *rec_wn, 
		   WN *parsfmt_wn, WN *fmtsrc_wn, 
		   WN *advance_wn, WN *size_wn, WN *varfmt )
{
  INT32 keytype;

  if (unit_wn != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_UNIT, unit_wn );
  if (items[IOC_IOSTAT] != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_IOSTAT, items[IOC_IOSTAT]);
  if (rec_wn != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_REC, rec_wn );
  if (parsfmt_wn != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_PARSFMT, parsfmt_wn );
  if (fmtsrc_wn != NULL) {
    Gen_Io_PutAddrWN ( block, st, FCR_CI_FMTSRC, fmtsrc_wn );
  }
  if (advance_wn != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_ADVANCE, advance_wn );
  if (size_wn != NULL)
    Gen_Io_PutAddrWN ( block, st, FCR_CI_SIZE, size_wn );
  if (varfmt != NULL) {
    /* Calvin TODO */
      fprintf( stderr, "Variable format not yet implemented\n" );
      abort();
  }
  if (items[IOC_KEY] != NULL) {
    keytype = IOC_KEY;
  } else if (items[IOC_KEYEQ] != NULL) {
    keytype = IOC_KEYEQ;
  } else if (items[IOC_KEYGE] != NULL) {
    keytype = IOC_KEYGE;
  } else if (items[IOC_KEYGT] != NULL) {
    keytype = IOC_KEYGT;
  } else
    keytype = 0;
  if (keytype) {
    fprintf( stderr, "Keyed I/O is not done\n" );
  } else {
    /* Calvin TODO: Need to clear the fields for non-indexed I/O */
  }
  if (items[IOC_KEYID] != NULL) {
  } else {
    /* Calvin TODO: Need to set the field to -1 for non-indexed I/O */
  }
}


/*  This routine is used to build up an I/O mask used as the second argument  */
/*  to the OPEN and INQUIRE runtimes.  Every user scalar variable (logical or */
/*  integer) which can be stored into by the runtime is represented by two    */
/*  bits in the iomask.  These two bits encode the size of the user variable. */

static void Build_Io_Mask ( INT32 * iomask, INT32 ioshift, WN * wn )
{
  TY_IDX  ty;
  INT32 mtype = 0;

  /*  Determine the basic type of the user variable.  */

  if (WN_operator(wn) == OPR_LDA  ||
      WN_operator(wn) == OPR_LDID ||
      WN_operator(wn) == OPR_ILOAD) {
    ty = WN_ty(wn);
    while (TY_kind(ty) == KIND_POINTER || TY_kind(ty) == KIND_ARRAY)
      if (TY_kind(ty) == KIND_POINTER)
	ty = TY_pointed(ty);
      else
        ty = TY_etype (ty);
    mtype = TY_mtype(ty);
  }

  /*  Based on the type, get the size code, shift it into position and add  */
  /*  it to the iomask being built.					    */

  *iomask |= (fio_maskcode[mtype] << ioshift);

}


/* ====================================================================
 *
 * WN *extract_calls (WN * block, WN * tree)
 *
 * Called by I/O lowerer to extract any nested calls or expressions which
 * require a store to a temporary.  Due to the semantics of F77 I/O such
 * expressions can not (in the general case) be extracted from within
 * the I/O statement when the tree is built.  On the other side, WHIRL
 * semantics does not allow a statement (or expression with side effects)
 * to be nested within an normal expression.  So the WHIRL rules were
 * slightly relaxed to allow CALL's within an I/O expression.  The case
 * of stores to temps is handled by a set of special, internal only,
 * INTRINSIC_OP's which indicate where a store to a temp is needed and
 * this routine does the extraction, temp generation, store and reload.
 *
 * ==================================================================== */

static WN *extract_calls ( WN * block, WN * tree )
{
  INT32 i;
  INT32 dtype;
  BOOL loadaddr;
  TY_IDX ttype;
  ST *temp;
  WN *kid0;
  PREG_NUM reg1, reg2;

  /* First process all kids in case there are multiple (nested) calls so
     that they will be done in the correct sequence.  */

  for (i = 0; i < WN_kid_count(tree); i++)
    WN_kid(tree,i) = extract_calls ( block, WN_kid(tree,i) );

  /*  If the current tree node is one of the special INTRINSIC_OP's, then
      extract the subtree, generate a temp and a store to that temp.  Place
      all of that at the block level and replace the subtree with an
      appropriate (re-)load operation.  Character functions are handled a
      little differently.  */

  if (WN_operator(tree) == OPR_INTRINSIC_OP) {

    kid0 = WN_kid0(tree);

    switch (WN_intrinsic(tree)) {

      case INTRN_U4I1ADRTMP:
      case INTRN_U8I1ADRTMP:	dtype    = MTYPE_I1;
				loadaddr = TRUE;
				break;

      case INTRN_U4I2ADRTMP:
      case INTRN_U8I2ADRTMP:	dtype    = MTYPE_I2;
				loadaddr = TRUE;
				break;

      case INTRN_U4I4ADRTMP:
      case INTRN_U8I4ADRTMP:	dtype    = MTYPE_I4;
				loadaddr = TRUE;
				break;

      case INTRN_U4I8ADRTMP:
      case INTRN_U8I8ADRTMP:	dtype    = MTYPE_I8;
				loadaddr = TRUE;
				break;

      case INTRN_U4F4ADRTMP:
      case INTRN_U8F4ADRTMP:	dtype    = MTYPE_F4;
				loadaddr = TRUE;
				break;

      case INTRN_U4F8ADRTMP:
      case INTRN_U8F8ADRTMP:	dtype    = MTYPE_F8;
				loadaddr = TRUE;
				break;

      case INTRN_U4FQADRTMP:
      case INTRN_U8FQADRTMP:	dtype    = MTYPE_FQ;
				loadaddr = TRUE;
				break;

      case INTRN_U4C4ADRTMP:
      case INTRN_U8C4ADRTMP:	dtype    = MTYPE_C4;
				loadaddr = TRUE;
				break;

      case INTRN_U4C8ADRTMP:
      case INTRN_U8C8ADRTMP:	dtype    = MTYPE_C8;
				loadaddr = TRUE;
				break;

      case INTRN_U4CQADRTMP:
      case INTRN_U8CQADRTMP:	dtype    = MTYPE_CQ;
				loadaddr = TRUE;
				break;

      case INTRN_U4VADRTMP:
      case INTRN_U8VADRTMP:	WN_Delete ( tree );
				WN_INSERT_BlockLast ( block, kid0 );
				if (WN_operator(WN_kid0(kid0)) != OPR_PARM)
				  return (WN_COPY_Tree (WN_kid0(kid0)));
				else
				 return (WN_COPY_Tree (WN_kid0(WN_kid0(kid0))));

      case INTRN_I4VALTMP:	dtype    = MTYPE_I4;
				loadaddr = FALSE;
				break;

      case INTRN_I8VALTMP:	dtype    = MTYPE_I8;
				loadaddr = FALSE;
				break;

      case INTRN_U4VALTMP:	dtype    = MTYPE_U4;
				loadaddr = FALSE;
				break;

      case INTRN_U8VALTMP:	dtype    = MTYPE_U8;
				loadaddr = FALSE;
				break;

      case INTRN_F4VALTMP:	dtype    = MTYPE_F4;
				loadaddr = FALSE;
				break;

      case INTRN_F8VALTMP:	dtype    = MTYPE_F8;
				loadaddr = FALSE;
				break;

      case INTRN_FQVALTMP:	dtype    = MTYPE_FQ;
				loadaddr = FALSE;
				break;

      case INTRN_C4VALTMP:	dtype    = MTYPE_C4;
				loadaddr = FALSE;
				break;

      case INTRN_C8VALTMP:	dtype    = MTYPE_C8;
				loadaddr = FALSE;
				break;

      case INTRN_CQVALTMP:	dtype    = MTYPE_CQ;
				loadaddr = FALSE;
				break;

      default:			return (tree);

    }

    WN_Delete ( tree );
    ttype = MTYPE_To_TY ( dtype );
    temp = Gen_Temp_Symbol ( ttype, "iotemp" );
    io_set_addr_saved_flag(temp);

    if (OPCODE_is_call(WN_opcode(kid0))) {

      WN_INSERT_BlockLast ( block, kid0 );

      if (WHIRL_Return_Info_On) {

	RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(dtype),
						   Use_Simulated);

	if (RETURN_INFO_count(return_info) <= 2) {

	  reg1 = RETURN_INFO_preg (return_info, 0);
	  reg2 = RETURN_INFO_preg (return_info, 1);
	}

	else
	  Fail_FmtAssertion ("extract_calls: more than 2 return registers");
      }
      else
	Get_Return_Pregs ( dtype, MTYPE_UNKNOWN, &reg1, &reg2 );

      WN_INSERT_BlockLast ( block, WN_Stid ( dtype, 0, temp, ttype,
				   WN_LdidPreg ( dtype, reg1 )));

    } else {

      WN_INSERT_BlockLast ( block, WN_Stid ( dtype, 0, temp, ttype, kid0 ));

    }

    if (loadaddr)
      return (WN_Lda ( Pointer_type, 0, temp ));
    else
      return (WN_Ldid ( dtype, 0, temp, ttype ));

  }

  /*  Just return the passed tree node because it's nothing special.  */

  return (tree);

}

static WN *
Create_fcd (WN *block, WN *kid1, WN *kid2)
{
   ST *st;
   WN *fcd;

   st = Get_IoStruct_ST ( block, FID_CRAY_FCD, FALSE );
   Gen_Io_PutAddrWN ( block, st, FCR_FCD_ADDR, WN_COPY_Tree(kid1));
   Gen_Io_PutFieldWN ( block, st, FCR_FCD_LEN, WN_COPY_Tree(kid2));
   fcd = WN_CreateMload( 0, TY_pointer(ST_type(st)),
             WN_CreateLda(opc_lda, 0, TY_pointer(ST_type(st)), st),
             WN_CreateIntconst ( OPC_U4INTCONST, fcd_size));
   return (fcd);
}

/* ====================================================================
 *
 * void copyout_temp_to_var ( WN * addr, ST *st, TY_IDX ty)
 *
 * For F90, for certain control list items, eg. iostat, exist, size, etc.,
 * the library expects the item to be an 32bit item. A temp is generated 
 * in get_32bit_cilist_item and then in this routine we generate a block
 * to copy all such temps back to the user variable.  
 *
 * ==================================================================== 
 */

static void copyout_temp_to_var ( WN * addr, ST *st, TY_IDX ty)
{
  WN * wn;

  if (TY_mtype(ty) == MTYPE_I1)
        wn = WN_CreateIstore ( OPC_I1ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I1), FALSE),
                       WN_CreateLdid ( OPC_I4I4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_I4) ), addr );
  else if (TY_mtype(ty) == MTYPE_U1)
        wn = WN_CreateIstore ( OPC_U1ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1), FALSE),
                       WN_CreateLdid ( OPC_U4U4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_U4) ), addr );
  else if (TY_mtype(ty) == MTYPE_I2)
        wn = WN_CreateIstore ( OPC_I2ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I2), FALSE),
                       WN_CreateLdid ( OPC_I4I4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_I4) ), addr );
  else if (TY_mtype(ty) == MTYPE_U2)
        wn = WN_CreateIstore ( OPC_U2ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_U2), FALSE),
                       WN_CreateLdid ( OPC_U4U4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_U4) ), addr );
  else if (TY_mtype(ty) == MTYPE_I8)
        wn = WN_CreateIstore ( OPC_I8ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8), FALSE),
                       WN_CreateLdid ( OPC_I8I4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_I4) ), addr );
  else if (TY_mtype(ty) == MTYPE_U8)
        wn = WN_CreateIstore ( OPC_U8ISTORE, 0,
                       Make_Pointer_Type(Be_Type_Tbl(MTYPE_U8), FALSE),
                       WN_CreateLdid ( OPC_U8U4LDID, 0, st,
                                       Be_Type_Tbl(MTYPE_U4) ), addr );
  else
    Fail_FmtAssertion("copyout_temp_to_var: unexpected type (%s)"
		      " in I/O processing", MTYPE_name(TY_mtype(ty)));

  if (copyout_block == NULL) 
    copyout_block = WN_CreateBlock();

  WN_INSERT_BlockLast ( copyout_block, wn );

}

/* ====================================================================
 *
 * WN * get_32bit_cilist_item(WN *cilist_item, TY_IDX ty)
 *
 * For F90, for certain control list items, eg. iostat, exist, size, etc.,
 * the library expects the item to be an 32bit item. If the cilist_item
 * is not a 32-bit item, a temp will be generated in this routine and 
 * copyout_temp_to_var will be called to generate a block that copies the temp
 * back to the user variable. Otherwise, the original item is returned.
 * The block, if generated, is grafted after the runtime call in 
 * process_iostat.
 *
 * ====================================================================
 */

static WN *
get_32bit_cilist_item(WN *cilist_item, TY_IDX ty)
{
  TYPE_ID mtype = TY_mtype(ty);
  ST *st;
  WN *wn;

  if (mtype != MTYPE_I4 && mtype != MTYPE_U4) {

     if (MTYPE_is_signed(mtype)) {
       st = Gen_Temp_Symbol( MTYPE_To_TY(MTYPE_U4), "temp_cilist_item");
       copyout_temp_to_var(cilist_item, st, ty);
       wn = WN_CreateLda (opc_lda, 0,
                          Make_Pointer_Type (MTYPE_To_TY(MTYPE_U4), FALSE),
                          st);
     } else {
       st = Gen_Temp_Symbol( MTYPE_To_TY(MTYPE_I4), "temp_cilist_item");
       copyout_temp_to_var(cilist_item, st, ty);
       wn = WN_CreateLda (opc_lda, 0,
                          Make_Pointer_Type (MTYPE_To_TY(MTYPE_I4), FALSE),
                          st);
     }
     return wn;
  } else {
     return cilist_item;
  }
}

/* ====================================================================
 *
 * void process_iostat ( WN ** block1, WN ** block2, BOOL flag,
 *			 WN * iostat, ST * err, ST * end, ST * eor,
 *                       BOOL zero_escape_freq )
 *
 * Called by the I/O lowerer to process all uses of the I/O status return.
 * The possible uses are IOSTAT=var, ERR=label, END=label or EOR=LABEL. This 
 * routine generates two blocks of code to deal with I/O status. The first 
 * (block1) is grafted after all but the last runtime call for the current I/O
 * statement.  The second (block2) is grafted after the final runtime
 * call.  Gen_Io_Call does the actual work of attaching these blocks of
 * code after the runtime call.  As an optimization, the flag argument is
 * TRUE if both blocks need to be generated (in the context of a particular
 * I/O statement, lower_io_statement knows if one or more runtime calls
 * will be required) or FALSE if only a degenerate version of block2 needs
 * to be generated.  zero_escape_freq is TRUE iff feedback is available
 * and feedback indicates that all branches to err, end, and eor have
 * exact zero frequency.
 *
 * ==================================================================== */

static void process_iostat ( WN **block1, WN **block2, BOOL flag, WN *iostat,
                             LABEL_IDX err, LABEL_IDX end, LABEL_IDX eor,
			     BOOL zero_escape_freq )
{
  PREG_NUM rreg1, rreg2;
  PREG_NUM pregnum;
  WN * wn;
  WN * iostatx;
  WN * test_label = NULL;
  ST * pregst;
  TY_IDX  iostatty;
  BOOL only_copyout_needed = FALSE;

  /*  If no error handling is specified, then return NULL blocks. */

#ifdef KEY
  // Under -IPA, different PUs may have different src_lang, but the driver
  // always passes -LANG:=ansi_c
  if (PU_f90_lang(Get_Current_PU())) {
#else
  if (Language == LANG_F90) {
#endif // KEY
    if (iostat == NULL &&
	end == (LABEL_IDX) 0 &&
	err == (LABEL_IDX) 0 &&
	eor == (LABEL_IDX) 0) {
      if (copyout_block) {
	 only_copyout_needed = TRUE;
      } else {
         *block1 = NULL;
         *block2 = NULL;
         return;
      }
    }
  } else {
    if (iostat == NULL && end == (LABEL_IDX) 0 && err == (LABEL_IDX) 0) {
      *block1 = NULL;
      *block2 = NULL;
      return;
    }
  }

  /*  Create BLOCK nodes to hold WHIRL trees.  */

  if (flag)
    *block1 = WN_CreateBlock();
  else
    *block1 = NULL;
  *block2 = WN_CreateBlock();


#ifdef KEY
  // Under -IPA, different PUs may have different src_lang, but the driver
  // always passes -LANG:=ansi_c
  if ((PU_f90_lang(Get_Current_PU())) && only_copyout_needed) {
#else
  if ((Language == LANG_F90) && only_copyout_needed) {
#endif // KEY
    if (flag)
       WN_INSERT_BlockLast ( *block1, WN_COPY_Tree(copyout_block) );
    WN_INSERT_BlockLast ( *block2, copyout_block );
    return;
  } 

  /*  Generate error test label if needed.  */

  if (flag)
    test_label = WN_CreateNewLabel();

  /*  Generate a preg to hold I/O status return.  */

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(MTYPE_I4),
					       Use_Simulated);

    if (RETURN_INFO_count(return_info) <= 2) {

      rreg1 = RETURN_INFO_preg (return_info, 0);
      rreg2 = RETURN_INFO_preg (return_info, 1);
    }

    else
      Fail_FmtAssertion ("process_iostat: more than 2 return registers");
  }

  else
    Get_Return_Pregs ( MTYPE_I4, MTYPE_UNKNOWN, &rreg1, &rreg2 );

  pregst = MTYPE_To_PREG ( MTYPE_I4 );
  pregnum = Create_Preg ( MTYPE_I4, "io_status");
  wn = WN_CreateStid ( OPC_I4STID, pregnum, pregst, Be_Type_Tbl(MTYPE_I4),
		       WN_CreateLdid ( OPC_I4I4LDID, rreg1, Int32_Preg,
				       Be_Type_Tbl(MTYPE_I4) ));
  if (flag)
    WN_INSERT_BlockLast ( *block1, WN_COPY_Tree(wn) );
  WN_INSERT_BlockLast ( *block2, wn );

#ifdef KEY
  // Under -IPA, different PUs may have different src_lang, but the driver
  // always passes -LANG:=ansi_c
  if ((PU_f90_lang(Get_Current_PU())) && copyout_block) {
#else
  if ((Language == LANG_F90) && copyout_block) {
#endif // KEY
    if (flag)
       WN_INSERT_BlockLast ( *block1, WN_COPY_Tree(copyout_block) );
    WN_INSERT_BlockLast ( *block2, copyout_block );
  }

  /*  Generate a store into a user variable if requested. */

  if (current_io_library == IOLIB_MIPS && iostat != NULL) {
    if (WN_operator(iostat) == OPR_LDA) {
      iostatty = WN_ty(iostat);
      while (TY_kind(iostatty) == KIND_POINTER)
        iostatty = TY_pointed(iostatty);
      if (TY_mtype(iostatty) == MTYPE_I1)
	wn = WN_CreateStid ( OPC_I1STID, 0, WN_st(iostat),
			     Be_Type_Tbl(MTYPE_I1),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(iostatty) == MTYPE_I2)
	wn = WN_CreateStid ( OPC_I2STID, 0, WN_st(iostat),
			     Be_Type_Tbl(MTYPE_I2),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(iostatty) == MTYPE_I4)
	wn = WN_CreateStid ( OPC_I4STID, 0, WN_st(iostat),
			     Be_Type_Tbl(MTYPE_I4),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(iostatty) == MTYPE_I8)
	wn = WN_CreateStid ( OPC_I8STID, 0, WN_st(iostat),
			     Be_Type_Tbl(MTYPE_I8),
			     WN_CreateLdid ( OPC_I8I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I8) ));
      else
	Fail_FmtAssertion("unexpected iostat type (%s) in"
			  " I/O processing",
			  MTYPE_name(TY_mtype(iostatty)));
    } else {
      iostatx = iostat;
      while (WN_operator(iostatx) == OPR_ARRAY)
	iostatx = WN_kid0(iostatx);
      iostatty = WN_ty(iostatx);
      while (TY_kind(iostatty) == KIND_POINTER ||
	     TY_kind(iostatty) == KIND_ARRAY)
        if (TY_kind(iostatty) == KIND_POINTER)
          iostatty = TY_pointed(iostatty);
        else
          iostatty = TY_AR_etype(iostatty);
      if (TY_mtype(iostatty) == MTYPE_I1)
	wn = WN_CreateIstore ( OPC_I1ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I1), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       iostat );
      else if (TY_mtype(iostatty) == MTYPE_I2)
	wn = WN_CreateIstore ( OPC_I2ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I2), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       iostat );
      else if (TY_mtype(iostatty) == MTYPE_I4)
	wn = WN_CreateIstore ( OPC_I4ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       iostat );
      else if (TY_mtype(iostatty) == MTYPE_I8)
	wn = WN_CreateIstore ( OPC_I8ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8), FALSE),
			       WN_CreateLdid ( OPC_I8I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I8) ),
			       iostat );
      else
	Fail_FmtAssertion("unexpected iostat type (%s) in I/O processing",
			  MTYPE_name(TY_mtype(iostatty)));
    }
    if (flag)
      WN_INSERT_BlockLast ( *block1, WN_COPY_Tree(wn) );
    WN_INSERT_BlockLast ( *block2, wn );
  }

  /*  Generate general error branch if needed.  */

  if (flag) {
    wn = WN_CreateTruebr ( WN_label_number(test_label),
			   WN_CreateExp2 ( OPC_I4I4NE,
					   WN_CreateLdid ( OPC_I4I4LDID,
							   pregnum, pregst,
							Be_Type_Tbl(MTYPE_I4) ),
					   WN_CreateIntconst ( OPC_I4INTCONST,
							       0 )));
    WN_INSERT_BlockLast ( *block1, wn );
    WN_INSERT_BlockLast ( *block2, test_label );
  }

  if (current_io_library == IOLIB_MIPS) {
     /*  Generate a conditional branch to an end statement if requested.  */
     
     if (end != (LABEL_IDX) 0) {
       LABEL_IDX end_label;
       end_label = end;
       wn = WN_CreateTruebr ( end_label,
   			   WN_CreateExp2 ( OPC_I4I4LT,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       0 )));
       WN_INSERT_BlockLast ( *block2, wn );

       // Update feedback
       if ( zero_escape_freq && Cur_PU_Feedback )
	 Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN, FB_FREQ_ZERO );
     }
   
     /*  Generate a conditional branch to an error statement if requested.  */
   
     if (err != (LABEL_IDX) 0) {
       LABEL_IDX err_label;
       err_label = err;
       if (!end && !iostat)
         wn = WN_CreateTruebr ( err_label,
   			   WN_CreateExp2 ( OPC_I4I4NE,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       0 )));
       else
         wn = WN_CreateTruebr ( err_label,
   			   WN_CreateExp2 ( OPC_I4I4GT,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       0 )));
       WN_INSERT_BlockLast ( *block2, wn );

       // Update feedback
       if ( zero_escape_freq && Cur_PU_Feedback )
	 Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN, FB_FREQ_ZERO );
     }
  } else {      /* IOLIB_CRAY */
     if (err != (LABEL_IDX) 0) {
       LABEL_IDX err_label;
       err_label = err;
       wn = WN_CreateTruebr ( err_label,
   			   WN_CreateExp2 ( OPC_I4I4EQ,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       1 )));
       WN_INSERT_BlockLast ( *block2, wn );

       // Update feedback
       if ( zero_escape_freq && Cur_PU_Feedback )
	 Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN, FB_FREQ_ZERO );
     }

     if (end != (LABEL_IDX) 0) {
       LABEL_IDX end_label;
       end_label = end;
       wn = WN_CreateTruebr ( end_label,
   			   WN_CreateExp2 ( OPC_I4I4EQ,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       2 )));
       WN_INSERT_BlockLast ( *block2, wn );

       // Update feedback
       if ( zero_escape_freq && Cur_PU_Feedback )
	 Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN, FB_FREQ_ZERO );
     }

     if (eor != (LABEL_IDX) 0) {
       LABEL_IDX eor_label;
       eor_label = eor;
       wn = WN_CreateTruebr ( eor_label,
   			   WN_CreateExp2 ( OPC_I4I4EQ,
   					   WN_CreateLdid ( OPC_I4I4LDID,
   							   pregnum, pregst,
   							Be_Type_Tbl(MTYPE_I4) ),
   					   WN_CreateIntconst ( OPC_I4INTCONST,
   							       3 )));
       WN_INSERT_BlockLast ( *block2, wn );

       // Update feedback
       if ( zero_escape_freq && Cur_PU_Feedback )
	 Cur_PU_Feedback->Annot( wn, FB_EDGE_BRANCH_TAKEN, FB_FREQ_ZERO );
     }
  }
}

/* ====================================================================
 *
 * void process_inqvar ( WN ** block, WN *var )
 *
 * Called by the I/O lowerer to process the return val for 
 * inquire(iolength=var). This routine generates a block of code to 
 * store the value returned by the inquire call into the user variable.
 * Gen_Io_Call does the actual work of attaching this block after the
 * runtime call.
 *
 *
 * ==================================================================== */

static void process_inqvar ( WN ** block, WN * var)
{
  PREG_NUM rreg1, rreg2;
  PREG_NUM pregnum;
  WN * wn;
  WN * varx;
  ST * pregst;
  TY_IDX  varty;

  if (var != NULL) {
    *block = WN_CreateBlock();

    /*  Generate a preg to hold I/O status return.  */

    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (Be_Type_Tbl(MTYPE_I4),
                                                 Use_Simulated);

      if (RETURN_INFO_count(return_info) <= 2) {

	rreg1 = RETURN_INFO_preg (return_info, 0);
	rreg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
        Fail_FmtAssertion ("process_inqvar: more than 2 return registers");
    }

    else
      Get_Return_Pregs ( MTYPE_I4, MTYPE_UNKNOWN, &rreg1, &rreg2 );

    pregst = MTYPE_To_PREG ( MTYPE_I4 );
    pregnum = Create_Preg ( MTYPE_I4, "io_status");
    wn = WN_CreateStid ( OPC_I4STID, pregnum, pregst, Be_Type_Tbl(MTYPE_I4),
		         WN_CreateLdid ( OPC_I4I4LDID, rreg1, Int32_Preg,
				         Be_Type_Tbl(MTYPE_I4) ));
    WN_INSERT_BlockLast ( *block, wn );

    if (WN_operator(var) == OPR_LDA) {
      varty = WN_ty(var);
      while (TY_kind(varty) == KIND_POINTER)
        varty = TY_pointed(varty);
      if (TY_mtype(varty) == MTYPE_I1)
	wn = WN_CreateStid ( OPC_I1STID, 0, WN_st(var),
			     Be_Type_Tbl(MTYPE_I1),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(varty) == MTYPE_I2)
	wn = WN_CreateStid ( OPC_I2STID, 0, WN_st(var),
			     Be_Type_Tbl(MTYPE_I2),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(varty) == MTYPE_I4)
	wn = WN_CreateStid ( OPC_I4STID, 0, WN_st(var),
			     Be_Type_Tbl(MTYPE_I4),
			     WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I4) ));
      else if (TY_mtype(varty) == MTYPE_I8)
	wn = WN_CreateStid ( OPC_I8STID, 0, WN_st(var),
			     Be_Type_Tbl(MTYPE_I8),
			     WN_CreateLdid ( OPC_I8I4LDID, pregnum, pregst,
					     Be_Type_Tbl(MTYPE_I8) ));
      else
	Fail_FmtAssertion("process_inqvar, LDA: unexpected var type (%s)"
			  " in I/O processing", MTYPE_name(TY_mtype(varty)));
    } else {
      varx = var;
      while (WN_operator(varx) == OPR_ARRAY)
	varx = WN_kid0(varx);
      varty = WN_ty(varx);
      while (TY_kind(varty) == KIND_POINTER ||
	     TY_kind(varty) == KIND_ARRAY)
        if (TY_kind(varty) == KIND_POINTER)
          varty = TY_pointed(varty);
        else
          varty = TY_AR_etype(varty);
      if (TY_mtype(varty) == MTYPE_I1)
	wn = WN_CreateIstore ( OPC_I1ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I1), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       var );
      else if (TY_mtype(varty) == MTYPE_I2)
	wn = WN_CreateIstore ( OPC_I2ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I2), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       var );
      else if (TY_mtype(varty) == MTYPE_I4)
	wn = WN_CreateIstore ( OPC_I4ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I4), FALSE),
			       WN_CreateLdid ( OPC_I4I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I4) ),
			       var );
      else if (TY_mtype(varty) == MTYPE_I8)
	wn = WN_CreateIstore ( OPC_I8ISTORE, 0,
			       Make_Pointer_Type(Be_Type_Tbl(MTYPE_I8), FALSE),
			       WN_CreateLdid ( OPC_I8I4LDID, pregnum, pregst,
					       Be_Type_Tbl(MTYPE_I8) ),
			       var );
      else
	Fail_FmtAssertion("process_inqvar: unexpected var type (%s)"
			  " in I/O processing", MTYPE_name(TY_mtype(varty)));
    }
    WN_INSERT_BlockLast ( *block, wn );
  }
}


static mINT32 search_implied_do_index ( WN *array_index, ST *implied_do_index )
{

    if ((WN_operator( array_index ) == OPR_CALL) ||
	(WN_operator( array_index ) == OPR_ICALL) ||
	(WN_operator( array_index ) == OPR_PICCALL) ||
	(WN_operator( array_index ) == OPR_INTRINSIC_CALL)) {
      return( 1 );
    } else if (WN_kid_count( array_index ) == 0) {
      if (WN_operator( array_index ) == OPR_LDID 
	  && WN_st( array_index ) == implied_do_index)
	return( 1 );
    } else if (WN_kid_count( array_index ) == 1) {
      return( search_implied_do_index( WN_kid0( array_index ),
				       implied_do_index));
    } else if (WN_kid_count( array_index ) == 2) {
      if (search_implied_do_index( WN_kid0( array_index ), implied_do_index))
	  return( 1 );
      if (search_implied_do_index( WN_kid1( array_index ), implied_do_index))
	  return( 1 );
    } else 
      /* return found for unrecognizable expression so no optimization
      ** will be done and to ensure correctness
      */
      return( 1 );
  return( 0 );
}


static FIOITEMTYPE get_FIT_type ( TY_IDX ty )
{
  FIOITEMTYPE type;
  INT32       mtype = TY_mtype(ty);
  BOOL        char_flag = TY_is_character(Ty_Table [ty]);
  BOOL        logical_flag = TY_is_logical(Ty_Table [ty]);

  switch (mtype) {

    case MTYPE_U1:
	type = FIT_CHARACTER;
	break;

    case MTYPE_I1:
	if (char_flag)
	  type = FIT_CHARACTER;
	else if (logical_flag)
	  type = FIT_LOGICAL1;
	else
	  type = FIT_INTEGER1;
	break;

    case MTYPE_I2:
	if (logical_flag)
	  type = FIT_LOGICAL2;
	else
	  type = FIT_INTEGER2;
	break;

    case MTYPE_I4:
	if (logical_flag)
	  type = FIT_LOGICAL4;
	else
	  type = FIT_INTEGER4;
	break;

    case MTYPE_I8:
	if (logical_flag)
	  type = FIT_LOGICAL8;
	else
	  type = FIT_INTEGER8;
	break;

    case MTYPE_U4:
	type = FIT_ADDRESS4;
	break;

    case MTYPE_U8:
	type = FIT_ADDRESS8;
	break;

    case MTYPE_F4:
	type = FIT_REAL4;
	break;

    case MTYPE_F8:
	type = FIT_REAL8;
	break;

    case MTYPE_F10:
    case MTYPE_FQ:
	type = FIT_REAL16;
	break;

    case MTYPE_C4:
	type = FIT_COMPLEX4;
	break;

    case MTYPE_C8:
	type = FIT_COMPLEX8;
	break;

    case MTYPE_CQ:
	type = FIT_COMPLEX16;
	break;

    case MTYPE_M:
	type = FIT_RECORD;
	break;

    case MTYPE_UNKNOWN:
	if (char_flag) {
	  type = FIT_CHARACTER;
	  break;
        }
	/*  Allow to fall through to error.  */

    default:
      Fail_FmtAssertion("get_FIT_type: unexpected type (%s) in I/O processing",
			MTYPE_name(mtype));

  }

  return type;

}



/* ====================================================================
 *
 * void lower_record_items (WN * block, FIOFORMATTYPE form, WN * iostat,
 *			    BOOL mode, TY_IDX  rty_idx, WN * addr, INT64 offset)
 *
 * Perform lowering of record io item list in fortran io statements.
 *
 * ==================================================================== */

static void lower_record_items ( WN * block, FIOFORMATTYPE form, WN * iostat,
				 BOOL mode, TY_IDX rty_idx, WN * addr, INT64 roffset)
{
  TY_IDX ty_idx;
  TY_IDX ety_idx;
  FIOITEMTYPE type;
  FIOITEMTYPE etype;
  ST *unit_ptr;
  INT64 offset;
  WN *wn;
  WN *wn1;
  WN *wn2;
  WN *wn3;
  INT64 size;
  INT64 nelem;
  ST *ctrst;
  ST *adrst;
  PREG_NUM ctrnum;
  PREG_NUM adrnum;
  WN *raddr;
  WN *recblk;

  TY& rty = Ty_Table [rty_idx];

  Is_True (TY_kind(rty) == KIND_STRUCT,
	   ("non record type passed to lower_record_items"));

  /*  Process all fields in the record.  */

  FLD_ITER fld_iter = Make_fld_iter (TY_fld(rty));

  do {
    FLD_HANDLE fld (fld_iter);

    ty_idx = FLD_type(fld);
    TY& ty = Ty_Table [ty_idx];
    offset = roffset + FLD_ofst(fld);

    switch (TY_kind(ty)) {

      case KIND_SCALAR:
      case KIND_POINTER:

	type = get_FIT_type ( ty_idx );
	if (mode) {
	  (void) Make_Pointer_Type ( ty_idx, FALSE );
	  if (offset <= INT32_MAX)
	    wn1 = WN_CreateLda ( opc_lda, offset, TY_pointer(ty_idx, FALSE),
				 WN_st(addr) );
	  else
	    wn1 = WN_Add ( Pointer_type,
			   WN_CreateLda ( opc_lda, 0, TY_pointer(ty_idx, FALSE),
					  WN_st(addr) ),
			   WN_CreateIntconst ( opc_const, offset ) );
	} else
	  wn1 = WN_Add ( Pointer_type, WN_COPY_Tree(addr),
			 WN_CreateIntconst ( opc_const, offset ) );
	if (mp_io) {
          unit_ptr = Get_UnitPointer_ST();
	  GEN_IO_CALL_3 ( block, fio_item_ops[form][type], iostat, NULL,
			  wn1, WN_CreateIntconst ( OPC_I4INTCONST, 1 ),
			  Make_IoAddr_WN(unit_ptr) );
	} else
	  GEN_IO_CALL_2 ( block, fio_item_ops[form][type], iostat, NULL,
			  wn1, WN_CreateIntconst ( OPC_I4INTCONST, 1 ));
	break;

      case KIND_ARRAY:

      {

	ety_idx = TY_AR_etype(ty);
        TY& ety = Ty_Table [ety_idx];
	etype = get_FIT_type ( ety_idx );
	switch (etype) {

	  case FIT_ADDRESS4:
	  case FIT_ADDRESS8:
	  case FIT_INTEGER1:
	  case FIT_INTEGER2:
	  case FIT_INTEGER4:
	  case FIT_INTEGER8:
	  case FIT_LOGICAL1:
	  case FIT_LOGICAL2:
	  case FIT_LOGICAL4:
	  case FIT_LOGICAL8:
	  case FIT_REAL4:
	  case FIT_REAL8:
	  case FIT_REAL16:
	  case FIT_COMPLEX4:
	  case FIT_COMPLEX8:
	  case FIT_COMPLEX16:
	    if (mode) {
	      (void) Make_Pointer_Type ( ety_idx, FALSE );
	      if (offset <= INT32_MAX)
		wn1 = WN_CreateLda ( opc_lda, offset, TY_pointer(ety_idx, FALSE),
				     WN_st(addr) );
	      else
		wn1 = WN_Add ( Pointer_type,
			       WN_CreateLda ( opc_lda, 0, TY_pointer(ety_idx, FALSE),
					      WN_st(addr) ),
			       WN_CreateIntconst ( opc_const, offset ) );
	    } else
	      wn1 = WN_Add ( Pointer_type, WN_COPY_Tree(addr),
			     WN_CreateIntconst ( opc_const, offset ) );
	    nelem = TY_size(ty) / TY_size(ety);
	    if (nelem <= INT32_MAX)
	      wn2 = WN_CreateIntconst ( OPC_I4INTCONST, nelem );
	    else
	      wn2 = WN_CreateIntconst ( OPC_I8INTCONST, nelem );
	    if (mp_io) {
              unit_ptr = Get_UnitPointer_ST();
	      GEN_IO_CALL_3 ( block, fio_item_ops[form][etype], iostat, NULL,
			      wn1, wn2, Make_IoAddr_WN(unit_ptr) );
	    } else
	      GEN_IO_CALL_2 ( block, fio_item_ops[form][etype], iostat, NULL,
			      wn1, wn2 );
	    break;

	  case FIT_CHARACTER:
	    if (mode) {
	      (void) Make_Pointer_Type ( ety_idx, FALSE );
	      if (offset <= INT32_MAX)
		wn1 = WN_CreateLda ( opc_lda, offset, TY_pointer(ety_idx, FALSE),
				     WN_st(addr) );
	      else
		wn1 = WN_Add ( Pointer_type,
			       WN_CreateLda ( opc_lda, 0, TY_pointer(ety_idx, FALSE),
					      WN_st(addr) ),
			       WN_CreateIntconst ( opc_const, offset ) );
	    } else
	      wn1 = WN_Add ( Pointer_type, WN_COPY_Tree(addr),
			     WN_CreateIntconst ( opc_const, offset ) );
	    if (TY_kind(ety) == KIND_ARRAY) {
	      size = TY_size(ety);
	      nelem = TY_size(ty) / size;
	    } else {
	      size = TY_size(ty);
	      nelem = 1;
	    }
	    if (size <= INT32_MAX)
	      wn2 = WN_CreateIntconst ( OPC_I4INTCONST, size );
	    else
	      wn2 = WN_CreateIntconst ( OPC_I8INTCONST, size );
	    if (nelem <= INT32_MAX)
	      wn3 = WN_CreateIntconst ( OPC_I4INTCONST, nelem );
	    else
	      wn3 = WN_CreateIntconst ( OPC_I8INTCONST, nelem );
	    if (mp_io) {
              unit_ptr = Get_UnitPointer_ST();
	      GEN_IO_CALL_4 ( block, fio_item_ops[form][etype], iostat, NULL,
			      wn1, wn2, wn3, Make_IoAddr_WN(unit_ptr) );
	    } else
	      GEN_IO_CALL_3 ( block, fio_item_ops[form][etype], iostat, NULL,
			      wn1, wn2, wn3 );
	    break;

	  case FIT_RECORD:
	    if (mode) {
	      (void) Make_Pointer_Type ( ety_idx, FALSE );
	      if (offset <= INT32_MAX)
		wn = WN_CreateLda ( opc_lda, offset, TY_pointer(ety_idx, FALSE),
				    WN_st(addr) );
	      else
		wn = WN_Add ( Pointer_type,
			      WN_CreateLda ( opc_lda, 0, TY_pointer(ety_idx, FALSE),
					     WN_st(addr) ),
			      WN_CreateIntconst ( opc_const, offset ) );
	    } else
	      wn = WN_Add ( Pointer_type, WN_COPY_Tree(addr),
			    WN_CreateIntconst ( opc_const, offset ) );
	    adrst = MTYPE_To_PREG ( Pointer_type );
	    adrnum = Create_Preg ( Pointer_type, "record_address");
	    WN_INSERT_BlockLast ( block,
				  WN_StidIntoPreg ( Pointer_type, adrnum, adrst,
						    wn ));
	    recblk = WN_CreateBlock();
	    ctrst = MTYPE_To_PREG ( MTYPE_I4 );
	    ctrnum = Create_Preg ( MTYPE_I4, "record_counter");
	    size = TY_size(ety);
	    nelem = TY_size(ty) / size;
	    WN_INSERT_BlockLast ( block,
				  WN_CreateDO (
				    WN_CreateIdname ( ctrnum, ctrst ),
				    WN_StidIntoPreg ( MTYPE_I4, ctrnum, ctrst,
				       WN_CreateIntconst ( OPC_I4INTCONST, 1 )),
				    WN_LE ( MTYPE_I4,
					    WN_LdidPreg ( MTYPE_I4, ctrnum ),
					    WN_CreateIntconst ( OPC_I4INTCONST,
								nelem )),
				    WN_StidIntoPreg ( MTYPE_I4, ctrnum, ctrst,
						      WN_Add ( MTYPE_I4,
						         WN_LdidPreg ( MTYPE_I4,
								       ctrnum ),
					     WN_CreateIntconst ( OPC_I4INTCONST,
								 (INT64)1 ))),
			 	    recblk, NULL ));
	    raddr = WN_LdidPreg ( Pointer_type, adrnum );
	    lower_record_items ( recblk, form, iostat, FALSE, ety_idx, raddr,
				 (INT64)0 );
	    WN_INSERT_BlockLast ( recblk,
				  WN_StidIntoPreg ( Pointer_type, adrnum, adrst,
				    WN_Add ( Pointer_type, raddr,
				      WN_CreateIntconst ( opc_const, size ))));
	    break;

	  default:
	    Fail_FmtAssertion("unexpected type (%s) in record I/O processing",
			      MTYPE_name(TY_mtype(ety)));

	}

	break;

      }

      case KIND_STRUCT:
	lower_record_items ( block, form, iostat, mode, ty_idx, addr, offset );
	break;

      case KIND_VOID:
	break;

      default:
	Fail_FmtAssertion("unexpected type (%s) in record I/O processing",
			  MTYPE_name(TY_mtype(ty)));

    }

  } while (! FLD_last_field (fld_iter++));
}


/* ====================================================================
 *
 * void lower_io_items (WN * block, WN * tree, FIOFORMATTYPE form,
 *		        WN * iostat, INT32 kid_first, INT32 kid_last)
 *
 * Perform lowering of the io item list in fortran io statements.
 *
 * ==================================================================== */

static void lower_io_items ( WN * block, WN * tree, FIOFORMATTYPE form,
			     WN * iostat, INT32 kid_first, INT32 kid_last )
{
  INT32 i;
  INT32 j;
  INT32 mtype;
  INT32 ntype;
  INT64 size;
  INT64 nelem;
  FIOITEMTYPE type;
  TY_IDX ty;
  TY_IDX ety;
  WN *item;
  WN *top_label;
  WN *cont_label;
  WN *start;
  WN *step;
  WN *end;
  WN *load_index;
  WN *raddr;
  WN *recblk;
  ST *pregst;
  ST *unit_ptr;
  ST *ctrst;
  ST *adrst;
  PREG_NUM pregnum;
  PREG_NUM ctrnum;
  PREG_NUM adrnum;
  IOITEM io_item;

  /*  Process all of the I/O items.  */

  for (i=kid_first; i<kid_last; i++) {

    item = WN_kid(tree,i);
    io_item = (IOITEM) WN_intrinsic(item);

    /*  If a real I/O item preprocess and determine type.  */

    if (io_item != IOL_IMPLIED_DO && io_item != IOL_IMPLIED_DO_1TRIP) {
      item = extract_calls ( block, item );
      if (io_item != IOL_ARRAY)
	type = get_FIT_type ( WN_ty(item) );
    }

    /*  Generate the appropriate I/O runtime call for the item.  */

    switch (io_item) {

      case IOL_ARRAY:
	  ety = TY_AR_etype(WN_ty(item));
	  type = get_FIT_type ( ety );
	  if (type == FIT_RECORD) {
	    adrst = MTYPE_To_PREG ( Pointer_type );
	    adrnum = Create_Preg ( Pointer_type, "record_address");
	    WN_INSERT_BlockLast ( block,
				  WN_StidIntoPreg ( Pointer_type, adrnum, adrst,
						    WN_kid0(item) ));
	    recblk = WN_CreateBlock();
	    ctrst = MTYPE_To_PREG ( MTYPE_I4 );
	    ctrnum = Create_Preg ( MTYPE_I4, "record_counter");
	    size = TY_size(ety);
	    nelem = TY_size(WN_ty(item)) / size;
	    WN_INSERT_BlockLast ( block,
				  WN_CreateDO (
				    WN_CreateIdname ( ctrnum, ctrst ),
				    WN_StidIntoPreg ( MTYPE_I4, ctrnum, ctrst,
				       WN_CreateIntconst ( OPC_I4INTCONST, 1 )),
				    WN_LE ( MTYPE_I4,
					    WN_LdidPreg ( MTYPE_I4, ctrnum ),
					    WN_CreateIntconst ( OPC_I4INTCONST,
								nelem )),
				    WN_StidIntoPreg ( MTYPE_I4, ctrnum, ctrst,
						      WN_Add ( MTYPE_I4,
						         WN_LdidPreg ( MTYPE_I4,
								       ctrnum ),
					     WN_CreateIntconst ( OPC_I4INTCONST,
								 (INT64)1 ))),
			 	    recblk, NULL ));
	    raddr = WN_LdidPreg ( Pointer_type, adrnum );
	    lower_record_items ( recblk, form, iostat, FALSE, ety, raddr,
				 (INT64)0 );
	    WN_INSERT_BlockLast ( recblk,
				  WN_StidIntoPreg ( Pointer_type, adrnum, adrst,
				    WN_Add ( Pointer_type, raddr,
				      WN_CreateIntconst ( opc_const, size ))));
	    WN_DELETE_Tree ( WN_kid1(item) );
	  } else {
	    if (mp_io) {
	      unit_ptr = Get_UnitPointer_ST();
	      GEN_IO_CALL_3 ( block, fio_item_ops[form][type], iostat, NULL,
			      WN_kid0(item), WN_kid1(item), 
			      Make_IoAddr_WN(unit_ptr) );
	    }
	    else
	      GEN_IO_CALL_2 ( block, fio_item_ops[form][type], iostat, NULL,
			      WN_kid0(item), WN_kid1(item) );
	  }
	  break;

      case IOL_CHAR:
	  if (mp_io) {
            unit_ptr = Get_UnitPointer_ST();
	    GEN_IO_CALL_4 ( block, fio_item_ops[form][type], iostat, NULL,
			  WN_kid0(item), WN_kid1(item), 
			  WN_CreateIntconst ( OPC_I4INTCONST, 1 ),
			  Make_IoAddr_WN(unit_ptr) );
	  }
	  else
	    GEN_IO_CALL_3 ( block, fio_item_ops[form][type], iostat, NULL,
			  WN_kid0(item), WN_kid1(item),
			  WN_CreateIntconst ( OPC_I4INTCONST, 1 ) );
	  break;

      case IOL_CHAR_ARRAY:
	  if (mp_io) {
            unit_ptr = Get_UnitPointer_ST();
	    GEN_IO_CALL_4 ( block, fio_item_ops[form][type], iostat, NULL,
			  WN_kid0(item), WN_kid1(item), 
			  WN_kid2(item),
			  Make_IoAddr_WN(unit_ptr) );
	  }
	  else
	    GEN_IO_CALL_3 ( block, fio_item_ops[form][type], iostat, NULL,
			  WN_kid0(item), WN_kid1(item), WN_kid2(item) );
	  break;

      case IOL_EXPR:
	  switch (type) {
	    case FIT_COMPLEX4:
		pregst = MTYPE_To_PREG ( MTYPE_C4 );
		pregnum = Create_Preg ( MTYPE_C4, "complex_io_item");
		WN_INSERT_BlockLast ( block,
				      WN_StidIntoPreg ( MTYPE_C4, pregnum,
							pregst,
							WN_kid0(item) ));
	        if (mp_io) {
                  unit_ptr = Get_UnitPointer_ST();
	          GEN_IO_CALL_3 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_F4REALPART,
				  WN_LdidPreg ( MTYPE_C4, pregnum )),
			  WN_CreateExp1 ( OPC_F4IMAGPART,
				  WN_LdidPreg ( MTYPE_C4, pregnum )),
			  Make_IoAddr_WN(unit_ptr) );
	        }
	        else
		  GEN_IO_CALL_2 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_F4REALPART,
				  WN_LdidPreg ( MTYPE_C4, pregnum )),
			  WN_CreateExp1 ( OPC_F4IMAGPART,
				  WN_LdidPreg ( MTYPE_C4, pregnum )));
		break;
	    case FIT_COMPLEX8:
		pregst = MTYPE_To_PREG ( MTYPE_C8 );
		pregnum = Create_Preg ( MTYPE_C8, "complex_io_item");

		WN_INSERT_BlockLast ( block,
				      WN_StidIntoPreg ( MTYPE_C8, pregnum,
							pregst,
							WN_kid0(item) ));
	        if (mp_io) {
                  unit_ptr = Get_UnitPointer_ST();
	          GEN_IO_CALL_3 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_F8REALPART,
				  WN_LdidPreg ( MTYPE_C8, pregnum )),
			  WN_CreateExp1 ( OPC_F8IMAGPART,
				  WN_LdidPreg ( MTYPE_C8, pregnum )),
			  Make_IoAddr_WN(unit_ptr) );
	        }
	        else
		  GEN_IO_CALL_2 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_F8REALPART,
				  WN_LdidPreg ( MTYPE_C8, pregnum )),
			  WN_CreateExp1 ( OPC_F8IMAGPART,
				  WN_LdidPreg ( MTYPE_C8, pregnum )));
		break;
	    case FIT_COMPLEX16:
		pregst = MTYPE_To_PREG ( MTYPE_CQ );
		pregnum = Create_Preg ( MTYPE_CQ, "complex_io_item");
		WN_INSERT_BlockLast ( block,
				      WN_StidIntoPreg ( MTYPE_CQ, pregnum,
							pregst,
							WN_kid0(item) ));
	        if (mp_io) {
                  unit_ptr = Get_UnitPointer_ST();
	          GEN_IO_CALL_3 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_FQREALPART,
				  WN_LdidPreg ( MTYPE_CQ, pregnum )),
			  WN_CreateExp1 ( OPC_FQIMAGPART,
				  WN_LdidPreg ( MTYPE_CQ, pregnum )),
			  Make_IoAddr_WN(unit_ptr) );
	        }
	        else
		  GEN_IO_CALL_2 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_CreateExp1 ( OPC_FQREALPART,
				  WN_LdidPreg ( MTYPE_CQ, pregnum )),
			  WN_CreateExp1 ( OPC_FQIMAGPART,
				  WN_LdidPreg ( MTYPE_CQ, pregnum )));
		break;
	    case FIT_RECORD:
		if (WN_operator(WN_kid0(item)) == OPR_LDA) {
		  lower_record_items ( block, form, iostat, TRUE,
				       WN_ty(item), WN_kid0(item),
				       (INT64)WN_offset(WN_kid0(item)) );
		  WN_Delete ( WN_kid0(item) );
		} else if (WN_operator(WN_kid0(item)) == OPR_LDID) {
		  lower_record_items ( block, form, iostat, FALSE,
				       WN_ty(item), WN_kid0(item), (INT64)0 );
		  WN_Delete ( WN_kid0(item) );
		} else {
		  pregst = MTYPE_To_PREG ( Pointer_type );
		  pregnum = Create_Preg ( Pointer_type, "record_address");

		  WN_INSERT_BlockLast ( block,
				        WN_StidIntoPreg ( Pointer_type, pregnum,
							  pregst,
							  WN_kid0(item) ));
		  raddr = WN_LdidPreg ( Pointer_type, pregnum );
		  lower_record_items ( block, form, iostat, FALSE,
				       WN_ty(item), raddr, (INT64)0 );
		  WN_Delete ( raddr );
		}
		break;
	    default:
	        if (mp_io) {
                  unit_ptr = Get_UnitPointer_ST();
	          GEN_IO_CALL_2 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_kid0(item), Make_IoAddr_WN(unit_ptr) );
	        }
	        else
		  GEN_IO_CALL_1 ( block, fio_value_ops[form][type], iostat, 
			  NULL, WN_kid0(item) );
	  }
	  break;

      case IOL_IMPLIED_DO:
      case IOL_IMPLIED_DO_1TRIP:
	  top_label = WN_CreateNewLabel();
	  cont_label = WN_CreateNewLabel();
	  WN_start(item) = extract_calls ( block, WN_start(item) );
	  WN_end(item)   = extract_calls ( block, WN_end(item) );
	  WN_step(item)  = extract_calls ( block, WN_step(item) );
	  ty = ST_type(WN_st(WN_index(item)));
	  if ( TY_kind(ty) != KIND_POINTER ) {
	    ntype = mtype = TY_mtype(ty);
	    if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
	      ntype = MTYPE_I4;
	    load_index = WN_Ldid ( mtype, WN_idname_offset(WN_index(item)),
				   WN_st(WN_index(item)), ty );
	    start = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
			      WN_st(WN_index(item)), ty, WN_start(item) );
	    step = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
			     WN_st(WN_index(item)), ty,
			     WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD, ntype,
							      MTYPE_V ),
					     WN_COPY_Tree ( load_index ),
					     WN_step(item) ));
	  } else {
	    ntype = mtype = TY_mtype(TY_pointed(ty));
	    if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
	      ntype = MTYPE_I4;
	    load_index = WN_Iload ( mtype, 0, TY_pointed(ty),
				    WN_Ldid ( Pointer_type,
					      WN_idname_offset(WN_index(item)),
					      WN_st(WN_index(item)), ty ));
	    start = WN_Istore ( mtype, 0, ty,
				WN_Ldid ( Pointer_type,
					  WN_idname_offset(WN_index(item)),
					  WN_st(WN_index(item)), ty ),
				WN_start(item) );
	    step = WN_Istore ( mtype, 0, ty,
			       WN_Ldid ( Pointer_type,
					 WN_idname_offset(WN_index(item)),
					 WN_st(WN_index(item)), ty ),
			       WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD, ntype,
								MTYPE_V ),
					       WN_COPY_Tree ( load_index ),
					       WN_step(item) ));
	  }
	  if ( WN_operator(WN_step(item)) == OPR_INTCONST ||
	       WN_operator(WN_step(item)) == OPR_CONST ) {
	    if ( ( WN_operator(WN_step(item)) == OPR_INTCONST &&
		   WN_const_val(WN_step(item)) >= 0 ) ||
		 ( WN_operator(WN_step(item)) == OPR_CONST &&
		   STC_val(WN_st(WN_step(item))).vals.ival.v0 >= 0 ) )
	      end = WN_LE ( ntype, load_index, WN_end(item) );
	    else
	      end = WN_GE ( ntype, load_index, WN_end(item) );
	  } else {
	    pregst = MTYPE_To_PREG ( Boolean_type );
	    pregnum = Create_Preg ( Boolean_type, "stoptemp");
	    WN_INSERT_BlockLast ( block,
				  WN_StidIntoPreg ( Boolean_type, pregnum,
						    pregst,
					    WN_GE ( ntype,
						 WN_COPY_Tree ( WN_step(item) ),
						 WN_Zerocon ( ntype ))));
	    end = WN_Select ( Boolean_type,
			      WN_LdidPreg ( Boolean_type, pregnum ),
			      WN_LE ( ntype, load_index, WN_end(item) ),
			      WN_GE ( ntype, WN_COPY_Tree (load_index),
				      WN_COPY_Tree (WN_end(item)) ));
	  }
	  WN_INSERT_BlockLast ( block, start );

/* 
In some special cases, the implied-do I/O is converted
into a single call to the runtime routine do_xxxx_1dim()
instead of having a loop generated to do I/O on one element
at a time.  The implied-DO loop must satisfy the following
conditions to be converted:
        - must have one and only one I/O item in the implied-do list
        - the item must be an array
        - every subscript of the array must be a simple variable
        - the leftmost index must be the same as the implied-do variable
        and all other indices must be different from the leftmost index.
        E.g:  (ARR(I,J), I=1,10) will be converted whereas
        (ARR(I,J), J=1,10) and (ARR(I,I), I=1,10) will not.
	- the array is not character type
	- there can be no nested calls in the subscript expression
The following arguments will be passed to the do_xxxx_1dim() runtime routine:
        1) the item type (for formatted I/O only)
        2) address of the array with the leftmost index replaced by
        constant 1.  Note that this could cause a problem once the
        -check_bounds option is implemented so once that is done
        3) The implied-do loop variable:  Its value will be modified
        appropriately when the I/O is completed.
        4) the lower-bound of the do-loop
        5) the upper bound of the do-loop
        6) the step size
        7) the length of one element in the implied-do (passed by value)
        8) the size of the implied-do variable (passed by value)
*/

	  /* 
	  ** Do the I/O optimization if and only if there is one item
	  ** in the implied-do loop list and it is a "real" array
	  */
	  /* Also don't do this for the -one-trip case as the logic
	  ** is different
	  */
	  if (io_item == IOL_IMPLIED_DO && WN_kid_count(item) == 5
	    && Implied_Do_Io_Opt && (WN_io_item(WN_kid(item,4)) != IOL_CHAR))
          {
	    WN *arr_item, *kid0;
	    WN *index1;
	    mINT32 base_offset;
	    mINT32 ndim;
	    TY_IDX arr_type;
	    WN *top_level_wn;

	    top_level_wn = WN_kid0(WN_kid(item,4));

	    if ( WN_operator(top_level_wn) != OPR_ILOAD )
	      arr_item = top_level_wn;
	    else if (WN_rtype(top_level_wn) != WN_desc(top_level_wn)) {
	      /* This could be an I2I4ILOAD for example, and we
		 can't use the base pointer of the original array,
		 since we need to load each element as an I2 and not
		 I4; so we skip the optimization */
	      goto no_optimize;
            } else {
	      /* An ILOAD with rtype == desc */
	      arr_item = WN_kid0(top_level_wn);
            }

	    if (WN_operator_is(arr_item, OPR_ARRAY)
		&& (((WN_operator_is((kid0= WN_kid0(arr_item)), OPR_LDA))
		      && (TY_kind ( WN_type (kid0)) == KIND_ARRAY)
		      && (TY_kind ( TY_AR_etype( WN_type(kid0))) !=
								   KIND_STRUCT))
		    || (WN_operator_is(kid0, OPR_LDID)
		      && (TY_kind ( WN_type_pointed(kid0)) == KIND_ARRAY)
		      && (TY_kind ( TY_AR_etype( WN_type_pointed(kid0))) !=
								  KIND_STRUCT)))
		&& WN_kid_count( arr_item ) == WN_num_dim( arr_item )*2+1)
	    {
	      ndim = WN_num_dim( arr_item );
	      for (j = ndim + 1; j < ndim*2; j++) {
		if (search_implied_do_index( WN_kid( arr_item, j ),
		    WN_st(WN_index(item))))
		  goto no_optimize;
	      }
	      /* get the first array index 'I' in (ARR(I,J,...)
	      ** It is normally in the form SUB(I, lower_bound)
	      */
	      index1 = WN_kid( arr_item, ndim*2 );
	      if (WN_kid_count( index1 ) == 0 
		    && WN_operator( index1 ) == OPR_LDID 
		    && WN_st( index1 ) == WN_st(WN_index(item))) {
		  base_offset = 0;
	      } else if (WN_kid_count( index1 ) == 2 &&
		    WN_kid_count( WN_kid0( index1 ) ) == 0
		    && WN_operator( WN_kid0(index1) ) == OPR_LDID
		    && WN_st( WN_kid0(index1) ) == WN_st(WN_index(item))
		    && WN_operator( WN_kid1( index1 ) ) == OPR_INTCONST
		    && (WN_operator( index1 ) == OPR_SUB
			|| WN_operator( index1 ) == OPR_ADD))
	          if (WN_operator( index1 ) == OPR_SUB)
		    base_offset = WN_const_val( WN_kid1( index1 ) );
		  else
		    base_offset = -WN_const_val( WN_kid1( index1 ) );
	      else
		  goto no_optimize;
	      /* 
	      ** Check that the base_offset is equal to the lower
	      ** bound of the left-most dimension which indicates that
	      ** the plain symbol 'i' is used for the index, and not
	      ** something like ARR(I+1), I = 1,10.
	      */
	      arr_type = TY_pointed( WN_ty( WN_kid0( arr_item ) ) );
	      if (TY_AR_const_lbnd(Ty_Table [arr_type],0) 
		    && TY_AR_lbnd_val(Ty_Table [arr_type],0) == base_offset) {
		  /* Substitute the implied-do variable index with 
		  ** a fixed constant 1
		  */
		  if (WN_kid_count( WN_kid(arr_item, ndim*2) ) == 0) {
		    WN_Delete ( WN_kid(arr_item, ndim*2) );
		    WN_kid(arr_item, ndim*2) = 
		       WN_CreateIntconst ( OPC_I4INTCONST, 1 );
		  } else {
		    WN_Delete ( WN_kid0(WN_kid(arr_item, ndim*2)) );
		    WN_kid0(WN_kid(arr_item, ndim*2)) =
		       WN_CreateIntconst ( OPC_I4INTCONST, 1 );
		  }

    		  io_item = (IOITEM) WN_intrinsic(arr_item);
		  type = get_FIT_type( TY_AR_etype(arr_type) );

	          if (mp_io) {
                    unit_ptr = Get_UnitPointer_ST();
	            Gen_Impld_Io_Calls( block, form, type,
			iostat, item, arr_item, Make_IoAddr_WN(unit_ptr) );
	          }
	          else
	            Gen_Impld_Io_Calls( block, form, type,
			iostat, item, arr_item, NULL );
		  break;
		  /*
		  }
		  */
	      }
	    }
	  }

no_optimize:
	  if ( io_item == IOL_IMPLIED_DO )
	    WN_INSERT_BlockLast ( block, WN_CreateGoto ( (ST_IDX) NULL,
						WN_label_number(cont_label) ));
	  WN_INSERT_BlockLast ( block, top_label );
	  lower_io_items ( block, item, form, iostat, 4, WN_kid_count(item) );
	  WN_INSERT_BlockLast ( block, step );
	  WN_INSERT_BlockLast ( block, cont_label );
	  WN_INSERT_BlockLast ( block,
				WN_CreateTruebr ( WN_label_number(top_label),
						  end ));
	  break;

      case IOL_LOGICAL:
	  if (mp_io) {
            unit_ptr = Get_UnitPointer_ST();
	    GEN_IO_CALL_2 ( block, fio_value_ops[form][type], iostat, NULL,
			  WN_kid0(item), Make_IoAddr_WN(unit_ptr) );
	  }
	  else
	    GEN_IO_CALL_1 ( block, fio_value_ops[form][type], iostat, NULL,
			  WN_kid0(item) );
	  break;

      case IOL_VAR:
	  if (type == FIT_RECORD) {
	    if (WN_operator(WN_kid0(item)) == OPR_LDA) {
	      lower_record_items ( block, form, iostat, TRUE,
				   WN_ty(item), WN_kid0(item),
				   (INT64)WN_offset(WN_kid0(item)) );
	      WN_Delete ( WN_kid0(item) );
	    } else if (WN_operator(WN_kid0(item)) == OPR_LDID) {
	      lower_record_items ( block, form, iostat, FALSE,
				   WN_ty(item), WN_kid0(item), (INT64)0 );
	      WN_Delete ( WN_kid0(item) );
	    } else {
	      pregst = MTYPE_To_PREG ( Pointer_type );
	      pregnum = Create_Preg ( Pointer_type, "record_addr");
	      WN_INSERT_BlockLast ( block,
				    WN_StidIntoPreg ( Pointer_type, pregnum,
						      pregst,
						      WN_kid0(item) ));
	      raddr = WN_LdidPreg ( Pointer_type, pregnum );
	      lower_record_items ( block, form, iostat, FALSE,
				   WN_ty(item), raddr, (INT64)0 );
	      WN_Delete ( raddr );
	    }
	  } else {
	    if (mp_io) {
              unit_ptr = Get_UnitPointer_ST();
	      GEN_IO_CALL_3 ( block, fio_item_ops[form][type], iostat, NULL,
			    WN_kid0(item),
			    WN_CreateIntconst ( OPC_I4INTCONST, 1 ),
			    Make_IoAddr_WN(unit_ptr) );
	    }
	    else
	      GEN_IO_CALL_2 ( block, fio_item_ops[form][type], iostat, NULL,
			    WN_kid0(item),
			    WN_CreateIntconst ( OPC_I4INTCONST, 1 ) );
	  }
	  break;
    }

  /*  Recycle the OPC_IO_ITEM node.  Note the the kid trees have been
      grafted onto the lowered tree so these nodes cannot be freed.  */

  WN_Delete ( item );

  }

}

/*===================================================
 *
 * Cray_Type_From_TY 
 *
 * return a Cray type from a TY. 
 *
 ====================================================
*/
extern INT32
Cray_Type_From_TY(TY_IDX typ)
{
  TYPE_ID ty;

  ty = TY_mtype( typ );
   switch (ty) {
    case MTYPE_U1:
    case MTYPE_I1: 
		  return ((Type_is_logical(typ)) ? 0x5300801 : 0x2300801); 

    case MTYPE_U2:
    case MTYPE_I2: 
                  return ((Type_is_logical(typ)) ? 0x5301002 : 0x2301002); 

    case MTYPE_U4:
    case MTYPE_I4: return ((Type_is_logical(typ)) ? 0x5302004 : 0x2302004);

    case MTYPE_U8:
    case MTYPE_I8: return ((Type_is_logical(typ)) ? 0x5304008 : 0x2304008);

    case MTYPE_F4: return (0x3302004);
    case MTYPE_F8: return (0x3304008);
    case MTYPE_FQ: return (0x3308010);
    case MTYPE_C4: return (0x4304004);
    case MTYPE_C8: return (0x4308008);
    case MTYPE_CQ: return (0x4310010);
   }
   DevAssert(0,("Do not know what to do with type"));
   /*NOTREACHED*/
   return(0);
}


static INT32
Dv_Type_From_TY(TY_IDX typ)
{
  TYPE_ID ty;

  ty = TY_mtype( typ );
#define    DVTYPE_UNUSED       0
#define    DVTYPE_TYPELESS     1
#define    DVTYPE_INTEGER      2
#define    DVTYPE_REAL         3
#define    DVTYPE_COMPLEX      4
#define    DVTYPE_LOGICAL      5
#define    DVTYPE_ASCII        6
#define    DVTYPE_DERIVEDBYTE  7
#define    DVTYPE_DERIVEDWORD  8
  
  if (MTYPE_is_integral(ty))
    return( (Type_is_logical(typ)) ? DVTYPE_LOGICAL : DVTYPE_INTEGER );
  else if (MTYPE_is_unsigned(ty))
    return( DVTYPE_LOGICAL );
  else if (MTYPE_is_complex(ty))
    return( DVTYPE_COMPLEX );
  else if (MTYPE_is_float(ty))
    return( DVTYPE_REAL );
  else if (MTYPE_is_str(ty))
    return( DVTYPE_ASCII );
  else if (MTYPE_is_pointer(ty))
    return( DVTYPE_DERIVEDWORD );
  else
    DevAssert(0,("Unknown type in Dv_Type_From_TY"));
  /*NOTREACHED*/
  return 0;
}


static WN *
Create_Dope_From_IoItem( WN *block, WN *item )
{
  /* 
  ** This function creates a dope vector for a character array when it is 
  ** used as an internal file.  The array can be treated as a single
  ** dimension array with the number of elements representing the number
  ** of records and the length of each element the size of each record.
  */
  INT64 info_word;
  ST *st;

  st = Get_IoStruct_ST ( block, FID_CRAY_DOPEVEC, FALSE );

  /* Put the appropriate values into the dope vector */
  /* Base address of the array */
  Gen_Iolist_PutAddrWN( block, st, 
			FIO_OFFSET(FCR_DV_BASE_PTR),
			FIO_TYPE(FCR_DV_BASE_PTR), 
			WN_kid0(item) );

  /* length of each element in bits */
  Gen_Iolist_PutFieldWN( block, st, 
			FIO_OFFSET(FCR_DV_BASE_LEN),
			FIO_TYPE(FCR_DV_BASE_LEN),
			WN_COPY_Tree(WN_kid1(item)) );

  /* flags and information word: consisting of assoc, ptr_alloc, p_or_a flag,
  ** and ndims */
  {
  /* info_word: containing these fields:
  ** assoc:1		=
  ** ptr_alloc:1	=
  ** p_or_a:2		=
  ** a_contig:1		=
  ** unused:
  ** n_dim:3		= number of dimensions
  */
    UINT64 assoc = 1;
    UINT64 a_contig = 1;
    dope_header_type *dh_ptr;

    a_contig = 1;

    info_word = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      dh_ptr = (dope_header_type *)&info_word;

      dh_ptr->assoc = assoc;
      dh_ptr->a_contig = a_contig;
      dh_ptr->n_dim = 1;
    } else {
      info_word = assoc << 63 | a_contig << 59 | 1;
    }
    Gen_Iolist_PutFieldConst( block, st, 
			FIO_OFFSET(FCR_DV_FLAG_INFO),
			FIO_TYPE(FCR_DV_FLAG_INFO), info_word);
  }

  /* f90type_t: data type and lengths */
  {
    /* O.K. this does not look great for execution speed but good
    ** for documenting all the fields in this messy argument */
    UINT64 type;
    UINT64 f90type_t_word;
    UINT64 int_len;

    f90_type_t *f90_type_ptr;
 
    f90type_t_word = 0;

      type = 6;
      int_len = 8; /* length in bits */

    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      f90_type_ptr = (f90_type_t *)&f90type_t_word;
      f90_type_ptr->type = type;
      f90_type_ptr->int_len = int_len;
    } else {
      f90type_t_word = (type << 24)
			    | (int_len << 8);
    }
      Gen_Iolist_PutFieldConst( block, st,
			FIO_OFFSET(FCR_DV_TYPE_LEN),
			FIO_TYPE(FCR_DV_TYPE_LEN), f90type_t_word);
  }

  /* Original base and length: always zeroes except for allocatable array */
  Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_ORIG_BASE),
			  FIO_TYPE(FCR_DV_ORIG_BASE), 0);
  Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_ORIG_SIZE),
			  FIO_TYPE(FCR_DV_ORIG_SIZE), 0);

  /* All the bound information on the array dimensions */
  Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_LB),
			FIO_TYPE(FCR_DV_DIM1_LB), (INT64) 1 );
  Gen_Iolist_PutFieldWN( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT),
		FIO_TYPE(FCR_DV_DIM1_EXTENT), WN_kid2(item) );
  Gen_Iolist_PutFieldWN( block, st, FIO_OFFSET(FCR_DV_DIM1_STRIDE),
			FIO_TYPE(FCR_DV_DIM1_STRIDE), WN_kid1(item));
  /* Create an address for the dope vector structure */
  return( Make_IoAddr_WN( st ) );
}


static WN *
Create_DopeVector_WN( WN *block, WN *arr_item, TY_IDX ity, TY_IDX  ety, BOOL impl_do )
/*
**	block: 	current block
**	arr_item:	could be either U4ARRAY node for implied-do or
**		an IO_ITEM <80,ARRAY> T<34,.anonymous.7> for a whole array
**		In either case, kid0 of arr_item would be in the form
**		U4LDA 0 S<1,1,arr> T<>
**	ity:	type of the array
**	ety:	type of the element involved.  This is not necessarily
**		the type of the array element but could be the type of
**		a field in an array of record.
**	impl_do: the dope vector is for implied-do
*/
{
  WN *wn, *ewn = NULL, *pe_wn = NULL;
  WN *kid0;
  ST *st;
  INT64 info_word;
  INT64 pe_const;
  INT32 ndims, i;
  INT32 const_stride;
  INT32 char_length;
  UINT64 dec_len, elen;

  /* 
  ** to do an array of structure, need to treat the array as an
  ** implied-do loop containing all the structure fields in its
  ** list 
  */
  st = Get_IoStruct_ST ( block, FID_CRAY_DOPEVEC, FALSE );

  if (TY_kind(ity) != KIND_ARRAY)
    DevAssert(0,("Expecting array kind in Create_DopeVector_WN"));
  
  kid0 = WN_kid0( arr_item );
  dec_len = TY_size( ety );
  /* Put the appropriate values into the dope vector */
  /* Base address of the array */
  /* Calculate the base address in the cases of (ARR(I+5),I=1,20)
  ** or (ARR(5,J), J=1,10), etc.) by substituting 1 for the correspoding
  ** implied-do variable.
  */
  Gen_Iolist_PutAddrWN( block, st, 
			FIO_OFFSET(FCR_DV_BASE_PTR),
			FIO_TYPE(FCR_DV_BASE_PTR), 
			impl_do ? arr_item : kid0 );

  /* length of each element in bits */
  if (TY_is_character( Ty_Table [ity] )) {
     char_length = TY_size( Ty_Table [ity] );
     if (char_length != 0) {
        Gen_Iolist_PutFieldConst( block, st,
                                  FIO_OFFSET(FCR_DV_BASE_LEN),
                                  FIO_TYPE(FCR_DV_BASE_LEN),
                                  char_length );  /* character length */
     } else {
        /* assumed-length character array, length is the second child of the arr_item node */
        Is_True((WN_operator(arr_item)==OPR_IO_ITEM && WN_io_item(arr_item)==IOL_CHAR_ARRAY),
                ("Bad character I/O item"));
        Gen_Iolist_PutFieldWN( block, st,
                               FIO_OFFSET(FCR_DV_BASE_LEN),
                               FIO_TYPE(FCR_DV_BASE_LEN),
                               WN_COPY_Tree(WN_kid1(arr_item)));  /* character length */
     }

  } else {
    INT64 elen = WN_element_size( arr_item );
    Gen_Iolist_PutFieldConst( block, st, 
			FIO_OFFSET(FCR_DV_BASE_LEN),
			FIO_TYPE(FCR_DV_BASE_LEN),
			elen * 8);
  }

  /* flags and information word: consisting of assoc, ptr_alloc, p_or_a flag,
  ** and ndims */
  ndims = TY_AR_ndims (ity);
  
  {
  /* info_word: containing these fields:
  ** assoc:1		=
  ** ptr_alloc:1	=
  ** p_or_a:2		=
  ** a_contig:1		=
  ** unused:
  ** n_dim:3		= number of dimensions
  */
    UINT64 assoc = 1; // indicates contains valid data
    UINT64 a_contig;

    dope_header_type *dh_ptr;

    info_word = 0;
    
    a_contig = (impl_do) ? 0 :  1;

    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      dh_ptr = (dope_header_type *)&info_word;
      dh_ptr->assoc = assoc;
      dh_ptr->a_contig = a_contig;
      dh_ptr->n_dim = ndims;
      if (TY_pointer( ity ))
	  dh_ptr->ptr_alloc = 1;
    } else {
     info_word = assoc << 63 | a_contig << 59 | ndims;
   /* ptr_alloc :  set if allocated by pointer  (can always be 0) */
     if (TY_pointer( ity ))
       info_word |= (UINT64) 1 << 62;
    }
//   /* p_or_a : pointer or allocatable array not important here */
//
    Gen_Iolist_PutFieldConst( block, st, 
			FIO_OFFSET(FCR_DV_FLAG_INFO),
			FIO_TYPE(FCR_DV_FLAG_INFO), info_word);
  }

  /* f90type_t: data type and lengths */
  {
    /* O.K. this does not look great for execution speed but good
    ** for documenting all the fields in this messy argument */
    UINT64 type;
    UINT64 dpflag = 0;	/* double precision flag for REAL*8 or REAL*16 ?? */
    UINT64 kind_of_star = 3; /* DVD_KIND_CONST */
    UINT64 int_len = TY_size( ety )*8; /* element length in bits */
    UINT64 f90type_t_word;

    f90_type_t *f90_type_ptr;
    f90type_t_word = 0;

      if (TY_is_character( ity )) {
	kind_of_star = 0;
	dec_len = 0;
      }
      type = (TY_is_character( ity )) ? 6 : Dv_Type_From_TY(ety);
      int_len = TY_size( ety )*8; /* length in bits */
      if (Target_Byte_Sex == LITTLE_ENDIAN) {
        f90_type_ptr = (f90_type_t *)&f90type_t_word;
        f90_type_ptr->type = type;
        f90_type_ptr->dpflag = dpflag;
        f90_type_ptr->kind_or_star = kind_of_star;
        f90_type_ptr->int_len = int_len;
        f90_type_ptr->dec_len = dec_len;
      } else {
     f90type_t_word = (type << 24)
		    | (dpflag << 23)
		    | (kind_of_star << 20)
		    | (int_len << 8)
		    | dec_len;
      }
      Gen_Iolist_PutFieldConst( block, st,
			FIO_OFFSET(FCR_DV_TYPE_LEN),
			FIO_TYPE(FCR_DV_TYPE_LEN), f90type_t_word);
  }

  /* Original base and length: always zeroes except for allocatable array */
  Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_ORIG_BASE),
			  FIO_TYPE(FCR_DV_ORIG_BASE), 0);
  Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_ORIG_SIZE),
			  FIO_TYPE(FCR_DV_ORIG_SIZE), 0);

  /* All the bound information on the array dimensions */
  const_stride = 1;
  elen = TY_size( TY_AR_etype( ity ) );
  for (i=0; i<ndims; i++) {
     ARB_HANDLE bnds = TY_arb(ity)[ndims -i - 1];
     /* Lower bound is always set to 1 */
     Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_LB+i*3),
			       FIO_TYPE(FCR_DV_DIM1_LB), (INT64) 1 );

    /* Extent */
    if (i == 0)
      ewn = WN_CreateIntconst( OPC_I8INTCONST, (elen > 4) ? elen / 4 : 1 );
    else if (const_stride)
      WN_const_val(ewn) *= pe_const;
    else
      ewn = WN_Mpy( MTYPE_I8, ewn, WN_COPY_Tree(pe_wn) );

    if (TY_is_character( ity )) {
      /* For character array, kid2 of item is the extent */
      if (WN_operator_is(WN_kid2(arr_item),OPR_INTCONST)) 
        Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT+i*3),
		FIO_TYPE(FCR_DV_DIM1_EXTENT), WN_const_val(WN_kid2(arr_item)) );
      else
        Gen_Iolist_PutFieldWN( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT+i*3),
		FIO_TYPE(FCR_DV_DIM1_EXTENT), WN_COPY_Tree(WN_kid2(arr_item) ));
    } else if (ARB_const_ubnd( bnds ) && ARB_const_lbnd( bnds )) {
      pe_const = ARB_ubnd_val( bnds ) - ARB_lbnd_val( bnds ) + 1;
      if (!const_stride)
        pe_wn = WN_CreateIntconst( OPC_I8INTCONST, pe_const );
        Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT+i*3),
		FIO_TYPE(FCR_DV_DIM1_EXTENT), 
		impl_do ? (UINT64) 1 : pe_const );
    } else {
      const_stride = 0;
      pe_wn = WN_Add( MTYPE_I8, 
		      WN_Sub( MTYPE_I8, Get_ARB_WN(bnds,ARB_UBOUND),Get_ARB_WN(bnds, ARB_LBOUND)),
		      WN_Intconst( MTYPE_I8,1));
      if (impl_do)
	 Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT+i*3),
				   FIO_TYPE(FCR_DV_DIM1_EXTENT), (UINT64) 1 );
      else
	 Gen_Iolist_PutFieldWN( block, st, FIO_OFFSET(FCR_DV_DIM1_EXTENT+i*3),
				FIO_TYPE(FCR_DV_DIM1_EXTENT), WN_COPY_Tree(pe_wn) );
    }
  
    /* Stride Multiplier */
    if (const_stride)
      Gen_Iolist_PutFieldConst( block, st, FIO_OFFSET(FCR_DV_DIM1_STRIDE+i*3),
			FIO_TYPE(FCR_DV_DIM1_STRIDE),
			(TY_is_character( ity )) 
			  ? WN_const_val( ewn ) * char_length
			  :  WN_const_val( ewn ) );
    else {
      Gen_Iolist_PutFieldWN( block, st, FIO_OFFSET(FCR_DV_DIM1_STRIDE+i*3),
			  FIO_TYPE(FCR_DV_DIM1_STRIDE), WN_COPY_Tree(ewn) );
    }
  }
  WN_Delete( ewn );
  if (pe_wn) 
    WN_Delete( pe_wn );
  /* Create an address for the dope vector structure */
  wn = Make_IoAddr_WN( st );
  return( wn );
}


static WN *
Replace_Impl_Idx( WN *expr )
/*
**	This function is used when the implied-do loop could
**	be called using a one-call model, and so it could assume
**	that all array index expression is a simple expression in
**	the form of I, I+-N, or N+I where N is a constant.  It will 
**	replace the refernce to the implied-do variable in the array 
**	indices with 1 so the array base (lower bound) could be assumed 
**	to be one.  Note that if the expression 'expr' is simple, such 
**	as I, then no replacement is done as it does not know the parent
**	of the replace node  It's then up to the caller to make the
**	replacement if the returned WN is the same as 'expr'
*/
{
  WN * kid;
  INT32 i;
  static BOOL replaced = 0;	/* flag to TRUE if the ST found has been
  				** replaced with 1
				*/
  if (WNOPR(expr) == OPR_TRIPLET) {
    /* will not happen for F77 */
    return( Replace_Impl_Idx( WN_kid0(expr) ) );
  } else if (WNOPR(expr) == OPR_LDID) {
    ST *idx_st = WN_st( expr );
    for (i = 0; i < num_impl; i++) {
      if (idx_st == impl_idx[i]) {
        replaced = 0;
	return( expr );
      }
    }
    return( NULL );
  } else if (WN_operator_is(expr,OPR_CONST) ||
               WN_operator_is(expr,OPR_INTCONST)) {
    return NULL;
  } else if ( WN_operator_is(expr,OPR_ADD) || WN_operator_is(expr,OPR_SUB)) { 
    if (kid = Replace_Impl_Idx(WN_kid0(expr))) {
      if (!replaced) {
        WN_kid0(expr) = WN_CreateIntconst ( OPC_I4INTCONST, 1 );
	replaced = 1;
      }
      return( kid );
    }
    if (kid = Replace_Impl_Idx(WN_kid1(expr))) {
      if (!replaced) {
        WN_kid1(expr) = WN_CreateIntconst ( OPC_I4INTCONST, 1 );
	replaced = 1;
      }
      return( kid );
    }
  }
  else {
    return( NULL );
  }
  return( NULL );
}


static void
Make_Cray_Io_Call( WN *block, FIOOPER form, WN *iostat1, WN *iostat2, 
		   WN *cilist_wn, WN *stack_wn, 
		   ST *iolist_st, UINT64 word)
{
  WN *iolist_wn;
  Gen_Iolist_PutFieldConst (block, iolist_st, 0, MTYPE_U8, word);
  iolist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(iolist_st)),
			        iolist_st);
  if (first_last & 1) {
    GEN_IO_CALL_3 ( block, form, WN_COPY_Tree(iostat1), WN_COPY_Tree(iostat2), 
		WN_COPY_Tree(cilist_wn), iolist_wn, WN_COPY_Tree(stack_wn));
  } else {
    GEN_IO_CALL_3 ( block, form, WN_COPY_Tree(iostat1), NULL,
		WN_COPY_Tree(cilist_wn), iolist_wn, WN_COPY_Tree(stack_wn));

  }
  first_last = 0;
}


static void
Create_Null_Call( WN *block, FIOOPER form, WN *cilist_wn, 
		WN *iostat1, WN *iostat2,
		WN *stack_wn )
/*
**	Make a null iolist call, with the first and/or last flag set.
**	For F77 this is needed only if an implied-do that gets turned into 
**	a DO loop is the firsst or the last item in the list.  It is
**	also used when there is an I/O statement with no items.   Since
**	the same iolist is used in this case we create a single static
**	ST for it.
*/
{
  UINT64 word;
  static ST *null_iolist_st;
  static TY_IDX null_iolist_ty_idx;
  static PU *null_iolist_current_pu = NULL;
  iolist_header_type *io_header_ptr;

  if (null_iolist_current_pu != Current_pu) {
    null_iolist_current_pu = Current_pu;

    Make_IoStruct_TY ( FID_CRAY_IOLIST );
    null_iolist_ty_idx = fiostruct_ty[FID_CRAY_IOLIST];
    TY& null_iolist_ty = Ty_Table [null_iolist_ty_idx];
    Set_TY_size(null_iolist_ty, 8);

    null_iolist_st = New_ST ();
    ST_Init (null_iolist_st,
             Save_Str ( fiostructid_info[FID_CRAY_IOLIST].name_local ),
             CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, null_iolist_ty_idx);
    io_set_addr_passed_flag(null_iolist_st);
  }

  word = 0;
  if (Target_Byte_Sex == LITTLE_ENDIAN) {
    io_header_ptr = (iolist_header_type *)&word;
    io_header_ptr->version = 1;
    io_header_ptr->iolfirst = (first_last == 2 || first_last == 3) ? 1 : 0;
    io_header_ptr->iollast = (first_last == 1 || first_last == 3) ? 1 : 0;
    io_header_ptr->ioetsize = 8 / Pointer_Size;
  } else {
  
 word = ( (UINT64) 1 << 61) | /* version */
              ( (UINT64) first_last << 32) | /* first_last flag*/
       ( (UINT64) 8 / Pointer_Size );

  }
  Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn, stack_wn, 
		   null_iolist_st, word);
}

static void
make_dope_vector_ty(int ndims_on_entry) {
      TY_IDX ty_idx;
      INT32 /* FIOSTRUCT */ i;
      FIOSTRUCTID id = FID_IOARRAY_ENTRY;
      FIOSTRUCT first = fiostructid_info[id].first;
      FIOSTRUCT last = fiostructid_info[id].last;
      int ndims = ndims_on_entry;

      /* Create the structure TY and fill in the appropriate fields. */

      TY& ty = New_TY(ty_idx);
      TY_Init (ty,FIO_SIZE(FID_IOARRAY_ENTRY) + Pointer_Size *ndims,
	       KIND_STRUCT, MTYPE_M, Save_Str ( fiostructid_info[id].name ));
 
      Set_TY_align(ty_idx, MTYPE_align_req(Pointer_type));

      for (i=first; i<=last; i++) {
	 FLD_HANDLE fld = New_FLD ();

	 if (i == first) Set_TY_fld(ty,fld);

	 if (Pointer_Size == 4) {
	    FLD_Init(fld, Save_Str ( fiostruct_info[i].name ),
		     Be_Type_Tbl ( fiostruct_info[i].type32 ),
		     fiostruct_info[i].offset32);
	 } else {
	    FLD_Init(fld, Save_Str ( fiostruct_info[i].name ),
		     Be_Type_Tbl ( fiostruct_info[i].type64 ),
		     fiostruct_info[i].offset64);
	 }
	 if (i == last && ndims == 0)
	    Set_FLD_last_field(fld); 
      }		  


      /* 
      ** Add the dimensions to make up a differnt type for each
      ** kind of dope vector with 0-MAX_DIM dimensions specified
      */
      while (ndims > 0) {
        FLD_HANDLE fld = New_FLD();
	if (Pointer_Size == 4) {
	   FLD_Init(fld, Save_Str ( fiostruct_info[i].name ),
		    Be_Type_Tbl ( fiostruct_info[i].type32 ),
		    fiostruct_info[i].offset32);
	} else {
	   FLD_Init(fld, Save_Str ( fiostruct_info[i].name ),
	            Be_Type_Tbl ( fiostruct_info[i].type64 ),
		    fiostruct_info[i].offset64);
	}
	i++;
	ndims--;
	if (ndims == 0) Set_FLD_last_field(fld);
      }

      /* Complete the TY entries, enter them in the global symbol table */
      /* and save the pointers.					*/

      dope_vector_ty[ ndims_on_entry ] = ty_idx;

}

static void
Add_To_Iolist(  FIOSTRUCTID id, FLD_HANDLE& last_field, INT32 *offset, INT32 ndims )
{
  TY_IDX ty_idx;

  if (id == FID_IOARRAY_ENTRY) {
    if (!dope_vector_ty[ ndims ]) {
       Fail_FmtAssertion ("dope_vector_ty not made yet: Add_To_Iolist");
    } else {
       ty_idx = dope_vector_ty[ ndims ];
       // *offset += ndims * Pointer_Size;
    }
  } else if (!fiostruct_ty[id]) {
    Make_IoStruct_TY ( id );
    ty_idx = fiostruct_ty[id];
  }
  else {
    ty_idx = fiostruct_ty[id];
  }

  /* Now enter the structure representing a new I/O entry into the I/O list */
  FLD_HANDLE fld = New_FLD();
  FLD_Init (fld, Save_Str(fiostructid_info[id].name_local),
	    ty_idx, *offset);

  *offset += FIO_SIZE(id) + ndims * Pointer_Size;
  last_field = fld;
}


static INT32
Create_Io_Entry ( WN *block, WN * item, WN *cilist_wn,  WN *stack_wn,
		  WN *iostat1, WN *iostat2,
		  FIOOPER form, ST **iolist_st, TY_IDX *iolist_ty,
		  INT32 *iolist_size,
		  FLD_HANDLE& last_field, INT32 *offset, INT32 *icount,
		  INT32 nested, BOOL need_loop)
/* 
** This function deals with a single I/O entry in the iolist.
** The iolist could be one in the main I/O statement, or in an
** implied-do which could be turned into an implied-do-entry
** or an implied-do which is turnd into a DO loop.   The need_loop
** flag indicates that the IOL_IMPLIED_DO will have to be converted
** into a DO loop by previous analysis due to its complexity.
**
** bloc		: current block
** item		: I/O item to be added
** cilist_wn	: current cilist
** iostat1	: 
** iostat2	: 
** form		: formatting value/flag
** iolist_st	: current iolist
** last_field	: last field in the I/O list	(updated)
** offset	: entry offset in the I/O list	(updated)
** nested:	: whether the item is inside an implieddo_entry
		: (i.e. converted to dope vector when appropriate)
		: or inside a do-loop generated from an implied-do/array.
** need_loop	: need to have a DO-loop generated for the implied-do
		: item because of dependency
*/
{
  TY_IDX ty;
  INT32 ioentsize, i, j;
#ifdef TEST_DOPE
  INT32 impldosize;
#endif
  INT32 val_type, cray_type = 0;
  INT32 header_offset;
  IOITEM io_item;
  WN *kid0, *wn;
  ST *arr_dims[MAX_DIM];
  TY_IDX aty;
  UINT64 word1, word2;
  WN *new_item;
  INT32 ndims;
  BOOL impl_do_arr = FALSE;
  BOOL struct_array = FALSE;
  BOOL read_mode;
  UINT64 word;
  INT32 nkids;
  ioentry_header_type *ioentry_header_ptr;
  ioarray_entry_type *ioentry_array_ptr;
  iolist_header_type *io_header_ptr;

  kid0 = WN_kid0( item );
  ty = WN_ty( item );
  io_item = (IOITEM) WN_intrinsic(item);

  switch (io_item) {
    case IOL_CHAR_ARRAY:
      /* In case of CHARACTER*N STR(M) */
      new_item = Create_DopeVector_WN( block, item, ty, TY_AR_etype(ty), FALSE );
      WN_Delete( item );
      item = new_item;
      kid0 = NULL;
      ty = WN_ty( item );
      val_type = 2;
      ioentsize = (Pointer_Size == 4) ? 5 : 3;
      break;

    case IOL_ARRAY:
      if (TY_kind(TY_AR_etype(ty)) == KIND_STRUCT) {
	/* turn the whole array of structures into an implied-do loop, 
	   with all the elements of the records included in the loop */
	val_type = 3;
	ioentsize = (Pointer_Size == 4) ? 6 : 5;
#ifdef TEST_DOPE
	impldosize = (Pointer_Size == 4) ? 6 : 5;
#endif
	struct_array = TRUE;
	break;
      }
      /* Turn the array into a dope vector */
      new_item = Create_DopeVector_WN( block, item, WN_ty(item), 
				TY_AR_etype(WN_ty(item)), FALSE );
      WN_Delete( item );
      item = new_item;
      kid0 = NULL;
      ty = WN_ty( item );
      val_type = 2;
      ioentsize = (Pointer_Size == 4) ? 5 : 3;
      break;

    case IOL_DOPE:  
      val_type = 2;
      ioentsize = (Pointer_Size == 4) ? 5 : 3;
    /* Calvin TODO */
      fprintf( stderr, "No can do IOL_DOPE, not applicable to F77 ?\n" );
      abort();
      break;

    case IOL_IMPLIED_DO:
    case IOL_IMPLIED_DO_1TRIP:
      val_type = 3;
      ioentsize = (Pointer_Size == 4) ? 6 : 5;
      break;

    case IOL_CHAR:
      /* 
      ** There are two types of IOL_CHAR, either simple character or
      ** character array.  Need to create a dope_vector in the case 
      ** of arrays.  Note that the array could be nested inside an
      ** implied-do loop.
      */
      if (nested == NESTED_DOPE && WNOPR(WN_kid0( item )) == OPR_ARRAY) {
	WN *arr_item = WN_kid0( item );;

	aty = WN_ty(item);
	ndims = TY_AR_ndims(Ty_Table[aty]);
        for (i = 0; i < ndims; i++) {
	    WN *idx = Replace_Impl_Idx( WN_kid( arr_item, ndims * 2 - i ) );
	    if (idx) {
	      for (j = 0; j < num_impl; j++) {
		if (WN_st( idx ) == impl_idx[j]) {
		  arr_dims[i] = WN_st( idx );
		  WN_Delete( idx );
		  break;
		}
	      }
	      if (idx == WN_kid( arr_item, ndims + i + 1 ))
		WN_kid( arr_item, ndims + i + 1 ) = 
		  WN_CreateIntconst( OPC_I4INTCONST, 1);
	    }
	    else
	      arr_dims[i] = NULL;
	  }
        new_item = Create_DopeVector_WN( block, arr_item, aty, TY_AR_etype(aty), TRUE );
        item = new_item;
	impl_do_arr = TRUE;
        kid0 = NULL;
        ioentsize = (Pointer_Size == 4) ? 5 : 3;
        ioentsize += ndims;
        ty = WN_ty( item );
        val_type = 2;
	cray_type = Cray_Type_From_TY(ty);
	break;
      } else {
	cray_type = 0x6000800;
        goto scalar;
      }
    case IOL_LOGICAL:
      cray_type = Cray_Type_From_TY(ty);
    case IOL_EXPR:
      if (TY_kind(ty) == KIND_STRUCT) {
	lower_f77_record_items( block, item, cilist_wn, stack_wn,
				iostat1, iostat2, form, iolist_st, 
				iolist_ty, iolist_size,
				last_field, offset, icount, nested, ty, 0 );
	return(0);
      } else if (TY_kind(ty) == KIND_SCALAR) {
	if (nested == NESTED_DOPE) {
	  WN *arr_item;

	  if (WN_kid_count(WN_kid0(item)) > 0) {
	    arr_item = WN_kid0( WN_kid0( item ) );
	    /* Logical array has a different tree structure */
	    if (io_item == IOL_LOGICAL && WN_kid_count(arr_item) > 0 
	      && WN_operator(WN_kid0( item )) == OPR_CVTL)
	      arr_item = WN_kid0( arr_item );
	    if (WN_operator(arr_item) == OPR_ARRAY) {
	      aty = TY_pointed( WN_ty( WN_kid0(arr_item) ));
              ndims = ARB_dimension (TY_arb(aty));
              for (i = 0; i < ndims; i++) {
	        WN *idx = Replace_Impl_Idx( WN_kid( arr_item, ndims * 2 - i ) );
	        /* If the index if the array is one of the impl-do loop
	        ** variable then save it to be used in the dovar array
	        ** of the ioarray_entry later
	        */
	        if (idx) {
		  for (j = 0; j < num_impl; j++) {
		    if (WN_st( idx ) == impl_idx[j]) {
		      arr_dims[i] = WN_st( idx );
		      WN_Delete( idx );
		      break;
		    }
		  }
		  if (idx == WN_kid( arr_item, ndims + i + 1 ))
		    WN_kid( arr_item, ndims + i + 1 ) = 
		      WN_CreateIntconst( OPC_I4INTCONST, 1);
	        }
	        else
		  arr_dims[i] = NULL;
	      }
	      /* 
	      ** The load/store instruction might contain an offset
	      ** as with record fields.
	      ** Need to transfer that offset to the base address of
	      ** the array item.
	      */
	      WN_lda_offset(WN_kid0(arr_item)) += WN_load_offset(WN_kid0(item));
              new_item = Create_DopeVector_WN( block, arr_item, aty, 
				WN_ty(item), TRUE );
              item = new_item;
              kid0 = WN_kid0( item );
              ty = WN_ty( item );
              val_type = 2;
              ioentsize = (Pointer_Size == 4) ? 5 : 3;
              ioentsize += ndims;
	      impl_do_arr = TRUE;
              cray_type = Cray_Type_From_TY(ty);
	      break;
	    }  /*  array element in implied-do */
	  }
	} /* NESTED_DOPE */
	else {
	  impl_do_arr = FALSE;
	}
        cray_type = Cray_Type_From_TY(ty);
      } else if (TY_kind(ty) == KIND_POINTER) {
	cray_type = Cray_Type_From_TY(ty);
      } else
         Fail_FmtAssertion(
		   "unexpected machine type (%s) for IOL_EXPR in IO Processing",
                   MTYPE_name(TY_mtype(ty)));
      goto scalar;
    case IOL_VAR:
	if (nested == NESTED_DOPE) {
	  WN *arr_item;
	 
	  arr_item = WN_kid0(item);
	  if (WN_operator(arr_item) == OPR_ARRAY) {
	    aty = TY_pointed( WN_ty( WN_kid0(arr_item) ));
            ndims = ARB_dimension (TY_arb (aty));
            for (i = 0; i < ndims; i++) {
	      WN *idx = Replace_Impl_Idx( WN_kid( arr_item, ndims * 2 - i ) );
	      /* If the index if the array is one of the impl-do loop
	      ** variable then save it to be used in the dovar array
	      ** of the ioarray_entry later
	      */
	      if (idx) {
		for (j = 0; j < num_impl; j++) {
		  if (WN_st( idx ) == impl_idx[j]) {
		    arr_dims[i] = WN_st( idx );
		    WN_Delete( idx );
		    break;
		  }
		}
		if (idx == WN_kid( arr_item, ndims + i + 1 ))
		  WN_kid( arr_item, ndims + i + 1 ) = 
		    WN_CreateIntconst( OPC_I4INTCONST, 1);
	      }
	      else
		arr_dims[i] = NULL;
	    }
            new_item = Create_DopeVector_WN( block, arr_item, aty, 
						TY_AR_etype(aty), TRUE );
	    cray_type = Cray_Type_From_TY(ty);
            item = new_item;
            kid0 = WN_kid0( item );
            ty = WN_ty( item );
            val_type = 2;
            ioentsize = (Pointer_Size == 4) ? 5 : 3;
            ioentsize += ndims;
	    impl_do_arr = TRUE;
	    break;
	  }
	  else if (TY_kind(ty) == KIND_STRUCT) {
	    lower_f77_record_items( block, item, cilist_wn, stack_wn,
				iostat1, iostat2, form, iolist_st, 
				iolist_ty, iolist_size,
				last_field, offset, 
				icount, nested, 
				TY_pointed(WN_ty(arr_item)), 0 );
	    return(0);
	  }
	  else {
	    cray_type = Cray_Type_From_TY(ty);
	    goto scalar;
	  }
      }
      if (TY_kind(ty) == KIND_STRUCT) {
	lower_f77_record_items( block, item, cilist_wn, stack_wn,
				iostat1, iostat2, form, iolist_st, 
				iolist_ty, iolist_size,
				last_field, offset, icount, nested, ty, 0 );
	return(0);
      }
scalar:
      val_type = 1;
      ioentsize = (Pointer_Size == 4) ? 6 : 4;
      break;
    default:
      Fail_FmtAssertion("Unexpected io item (%d)", io_item);
  }

  header_offset = *offset;
  *icount += 1;

  if (val_type == 1) {

    /* ioscalar entry */
    Add_To_Iolist( FID_IOSCALAR_ENTRY, last_field, offset, 0 );

    word1 = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      ioentry_header_ptr = (ioentry_header_type *)&word1;
      ioentry_header_ptr->valtype = val_type;
      ioentry_header_ptr->ioentsize = ioentsize;
    } else {
    /* io_entry header */
    word1 = ((UINT64) val_type << 56) |	/* val type */
          ((UINT64) ioentsize);
    }
    Gen_Iolist_PutFieldConst (block, *iolist_st, header_offset, MTYPE_U8, word1);

    /* F90_type_t entry */
    if (cray_type == 0) {
      word2 =  (UINT64) Cray_Type_From_TY(ty);
    } else
      word2 = (UINT64) cray_type;
    Gen_Iolist_PutFieldConst( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOSCALAR_TYPE_T), 
		FIO_TYPE(FCR_IOSCALAR_TYPE_T), word2);

    /* iovar_address */
    if (TY_kind(ty) == KIND_POINTER) {
      ST *tmp_st;
      WN *stid;
      tmp_st = Gen_Temp_Symbol( MTYPE_To_TY(Pointer_type), "temp-expr");
      stid = WN_Stid (Pointer_type, 0, tmp_st, ST_type(tmp_st), kid0);
      WN_INSERT_BlockLast(block, stid);
      wn = create_pointer_to_node (block, stid, (TY_IDX) 0, TRUE);
  
    } else if (WN_operator(kid0) == OPR_LDID 
         && TY_kind(WN_ty(kid0)) == KIND_POINTER) {
      /* 
      ** For dummy arguments, use the address value directly
      ** create_pointer_to_node() won't handle it
      */
      wn = WN_COPY_Tree( kid0 );
    } else {
      wn = create_pointer_to_node (block, kid0, ty, TRUE);
    }
    Gen_Iolist_PutAddrWN ( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOSCALAR_ADDR), 
		FIO_TYPE(FCR_IOSCALAR_ADDR), wn );
    if (io_item == IOL_CHAR) {
      /* Add character length */
      Gen_Iolist_PutFieldWN ( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOSCALAR_CHAR_LEN),
		FIO_TYPE(FCR_IOSCALAR_CHAR_LEN), WN_kid1( item ) );
    }
  } else if (val_type == 2) {
    /* ioarray entry */
    Add_To_Iolist( FID_IOARRAY_ENTRY, last_field, offset, 
		impl_do_arr ? ndims : 0 );

    /* item is simply the address of the dope vector */
    /* Address of Dope vector */
    Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOARRAY_DV_ADDR), 
		FIO_TYPE(FCR_IOARRAY_DV_ADDR), item );

    /* 64-bit flags for indexed & bound-checking */
    /* Calvin TODO: need to take care of bound checking flag */
    /* indflag, boundchk */
    word1 = 0;
    ioentry_array_ptr = (ioarray_entry_type *)&word1;

    ioentry_array_ptr->indflag = (impl_do_arr ?  1 : 0);

    Gen_Iolist_PutFieldConst( block, *iolist_st, 
	header_offset + FIO_OFFSET(FCR_IOARRAY_FLAG), 
	FIO_TYPE(FCR_IOARRAY_FLAG), 
	word1);
// ^ used to be ...
//    Gen_Iolist_PutFieldConst( block, *iolist_st, 
//	header_offset + FIO_OFFSET(FCR_IOARRAY_FLAG), 
//	FIO_TYPE(FCR_IOARRAY_FLAG), 
//	impl_do_arr ? (UINT64) 1 << 63 : (UINT64) 0 );
				
    /* Addresses of indices */
    if (impl_do_arr) {
      for (i = 0; i < ndims; i++) {
	/* Put the address of the array subscript here if and only if
	** it is used as an implied-do variariable */
	if (arr_dims[i])
          Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOARRAY_IDX1+i), 
		FIO_TYPE(FCR_IOARRAY_IDX1), 
		Make_IoAddr_WN( arr_dims[i] ));
	else
	  Gen_Iolist_PutFieldConst( block, *iolist_st, 
	    header_offset + FIO_OFFSET(FCR_IOARRAY_IDX1+i), 
	    FIO_TYPE(FCR_IOARRAY_IDX1), 0 );
      }
    }
    word1 = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      ioentry_header_ptr = (ioentry_header_type *)&word1;
      ioentry_header_ptr->valtype = val_type;
      ioentry_header_ptr->ioentsize = ioentsize;   
    } else {
      word1 = ((UINT64) val_type << 56) |	/* val type */
            ((UINT64) ioentsize);
    }
    Gen_Iolist_PutFieldConst (block, *iolist_st, header_offset, 
		FIO_TYPE(FCR_IOARRAY_ENTRY), word1);
  } else if (val_type == 3) {
    ST *idx_st[MAX_DIM];

    /* implied-do entry loop.  Either need to generate a DO-loop if
    ** need_loop is set, else generate an implied-do entry */
    if (!need_loop) {

#ifdef TEST_DOPE
      ARB	*bnds;
      /* 
      ** This almost works but doesn't work yet.  The one remaining thing
      ** to do is to convert the record into a collection of dope vectors,
      ** one dope vector for each field
      */
      if (struct_array) {
	ari = TY_arinfo(ty);
	ndims = ARI_ndims(ari);
	nkids = ndims * 2 + 1;
	kid0 = WN_Create( Pointer_Size == 4 ? OPC_U4ARRAY : OPC_U8ARRAY, nkids );
	WN_element_size(kid0) = TY_size(TY_AR_etype(TY_pointed(WN_ty(WN_kid0(item)))));
	WN_kid0(kid0) = WN_COPY_Tree( WN_kid0(item) );
	for (i = 0, ioffset = header_offset; i < ndims; i++, ioffset += 8 ) {
	  /* start of an implied-do entry */
	  header_offset = ioffset;
          Add_To_Iolist( FID_IOIMPLIEDDO_ENTRY, last_field, &ioffset, 0 );
	  /* address of loop variable */
          idx_st = Gen_Temp_Symbol( MTYPE_To_TY(MTYPE_I4), "struct_arr_idx");
	  impl_idx[num_impl++] = idx_st;
	  Gen_Iolist_PutAddrWN( block, *iolist_st,
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_VAR_ADDR),
		FIO_TYPE(FCR_IOIMPLIEDDO_VAR_ADDR),
		Make_IoAddr_WN( idx_st ));
	  bnds = &ARI_bnd( ari, i);
	  /* address of beginning count of loop */
	  if (ARB_const_lbnd(*bnds))
	    wn = WN_CreateIntconst( OPC_I4INTCONST, ARB_lbnd_val( *bnds ) );
	  else
	    wn = WN_COPY_Tree( ARB_lbnd_tree( *bnds ) );
	  Gen_Iolist_PutAddrWN( block, *iolist_st,
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_BEGIN_CNT),
		FIO_TYPE(FCR_IOIMPLIEDDO_BEGIN_CNT), 
		create_pointer_to_node( block, wn, NULL, TRUE ));

	  /* UARRAY kids */
	  WN_kid(kid0, nkids-i-1) = WN_Sub( Int_Type,
		WN_Ldid(Int_Type, 0, idx_st, ST_type(idx_st) ), 
		wn );
	  if (ARB_const_ubnd(*bnds) && ARB_const_lbnd(*bnds))
	    WN_kid(kid0, nkids-i-ndims-1) = 
		WN_CreateIntconst( OPC_I4INTCONST,
			ARB_ubnd_val( *bnds ) - ARB_lbnd_val( *bnds ) + 1 );
	  else
	    WN_kid(kid0, nkids-i-ndims-1) = 
		WN_Ldid(Int_Type, 0, WN_st(ARB_ubnd_tree( *bnds )),
			ST_type(WN_st(ARB_ubnd_tree( *bnds ))));

	  /* address of ending count of loop */
	  if (ARB_const_ubnd(*bnds)) {
	    wn = WN_CreateIntconst( OPC_I4INTCONST, ARB_ubnd_val( *bnds ) );
	    wn = create_pointer_to_node( block, wn, NULL, TRUE );
	  } else
	    wn = create_pointer_to_node( block, ARB_ubnd_tree( *bnds ), NULL, TRUE );
	  Gen_Iolist_PutAddrWN( block, *iolist_st,
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_END_CNT),
		FIO_TYPE(FCR_IOIMPLIEDDO_END_CNT), wn );
	  /* address of increment of loop */
	  wn = WN_CreateIntconst( OPC_I4INTCONST, 1 );
	  wn = create_pointer_to_node( block, wn, NULL, TRUE );
	  Gen_Iolist_PutAddrWN( block, *iolist_st,
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_INC_CNT),
		FIO_TYPE(FCR_IOIMPLIEDDO_INC_CNT), wn );
	  if (i < ndims-1) {
	    /* Starting a "new" iolist for the implied-do and append it to
	    ** the old iolist
	    */
	    field = New_FLD (1, TRUE);
	    FLD_name(field) = Save_Str("implied_do_iolist_header");
	    FLD_ofst(field) = ioffset;
	    FLD_type(field) = Be_Type_Tbl (MTYPE_U8);
	    FLD_next(*last_field) = field;
	    *last_field = field;
	  }
	}
	/* All implied-do entries for the array have been created,
	** expand the record to include all the fields as items
	** in the implied-do list
	*/
	read_mode = form == FIO_CR_READ_UNFORMATTED
			|| form == FIO_CR_READ_FORMATTED
			|| form == FIO_CR_READ_NAMELIST;
	wn = WN_CreateIoItem1( read_mode ? IOL_VAR : IOL_EXPR, 
			WN_COPY_Tree( kid0 ), NULL );
	ty = TY_AR_etype(TY_pointed(WN_ty(WN_kid0(item))));
	/*WN_set_ty(wn, TY_AR_etype(TY_pointed(WN_ty(WN_kid0(item)))));*/
	ncount = 0;
	*offset = ioffset;
	/* 
	** TODO: can't simply do this.  Each field must be turned into
	** IOL_DOPE with appropriate step size and index like F90 for 
	** it to be "stepped" correctly by the implied-do entry.
	*/
	lower_f77_record_items( block, wn, cilist_wn, stack_wn,
				iostat1, iostat2, form, iolist_st, 
				iolist_ty, iolist_size,
				last_field, offset, 
				&ncount, NESTED_DOPE, ty, 0 );
	ioffset -= 8;
	for (i = 0; i < ndims; i++) {
	  if (i > 0) {
	    /* implied_do entry word */
            word = 0;
	    if (Target_Byte_Sex == LITTLE_ENDIAN) {
              ioentry_header_ptr = (ioentry_header_type *)&word;
              ioentry_header_ptr->valtype = 3;
              ioentry_header_ptr->ioentsize = ioentsize;
	    } else {
	      word = ((UINT64) 3 << 56) | ((UINT64) ioentsize);
	    }
	    Gen_Iolist_PutFieldConst( block, *iolist_st, ioffset+8,
				MTYPE_U8, word);
	  }
	  /* version_word */
	  ioentsize += 8 / Pointer_Size;
	  word = ( (UINT64) 1 << 61) | /* version */
		( (UINT64) ncount <<  16) | 
		( (UINT64) ioentsize );
	  Gen_Iolist_PutFieldConst (block, *iolist_st, 
			ioffset, MTYPE_U8, word);
	  ncount = 1;
	  ioentsize += impldosize;
	  ioffset -= (impldosize*Pointer_Size+8);
	}
	/* implied_do entry word */
        word = 0;
	if (Target_Byte_Sex == LITTLE_ENDIAN) {
          ioentry_header_ptr = (ioentry_header_type *)&word;
          ioentry_header_ptr->valtype = 3;
          ioentry_header_ptr->ioentsize = ioentsize;
	} else {
	  word = ((UINT64) 3 << 56) | ((UINT64) ioentsize);
	}
	Gen_Iolist_PutFieldConst( block, *iolist_st, ioffset+8,
			MTYPE_U8, word);
	return( ioentsize-impldosize-8/Pointer_Size );
      }
#endif /* TEST_DOPE */

      Add_To_Iolist( FID_IOIMPLIEDDO_ENTRY, last_field, offset, 0 );
      /* Add the implied-do index variable to the list of nested 
      ** implied-do indices
      */
      impl_idx[num_impl++] = WN_st(WN_index(item));
      /* address of loop variable */
      Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_VAR_ADDR), 
		FIO_TYPE(FCR_IOIMPLIEDDO_VAR_ADDR), 
		Make_IoAddr_WN( WN_st(WN_index(item))));
  
      /* address of beginning count of loop */
        wn = create_pointer_to_node( block, WN_start(item), (TY_IDX) 0, TRUE );
        Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_BEGIN_CNT), 
		FIO_TYPE(FCR_IOIMPLIEDDO_BEGIN_CNT), wn );

      /* address of ending count of loop */
      wn = create_pointer_to_node( block, WN_end(item), (TY_IDX) 0, TRUE);
      Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_END_CNT), 
		FIO_TYPE(FCR_IOIMPLIEDDO_END_CNT), wn );
      
      /* address of increment of loop */
      wn = create_pointer_to_node( block, WN_step(item), (TY_IDX) 0, TRUE );
      Gen_Iolist_PutAddrWN( block, *iolist_st, 
		header_offset + FIO_OFFSET(FCR_IOIMPLIEDDO_INC_CNT), 
		FIO_TYPE(FCR_IOIMPLIEDDO_INC_CNT), wn );
  
      /* An ioentsize for an implied-do entry includes the sizes of
      ** all items in the implied-do list
      */
      ioentsize += lower_f77_io_items( block, item, cilist_wn, 
			iostat1, iostat2, form, NESTED_DOPE, 
			offset, 4, WN_kid_count(item) );

      /* replace the ioentry header which had the wrong ioentsize value now
         that we know the real ioentsize for the implied-do */
      word1 = 0;
      if (Target_Byte_Sex == LITTLE_ENDIAN) {
        ioentry_header_ptr = (ioentry_header_type *)&word1;
        ioentry_header_ptr->valtype = 3;
        ioentry_header_ptr->ioentsize = ioentsize;
      } else {
        word1 = ((UINT64) val_type << 56) |	/* val type */
              ((UINT64) ioentsize);
      }
      Gen_Iolist_PutFieldConst (block, *iolist_st, header_offset, 
		FIO_TYPE(FCR_IOIMPLIEDDO_ENTRY), word1);
      num_impl--;
    } else {
      INT32 impl_offset = 0;
      INT32 ntype, mtype;
      WN *top_label;
      WN *cont_label;
      WN *start, *step, *end;
      WN *load_index;
      ST *pregst;
      PREG_NUM pregnum;

      if (struct_array) {
        /* Create DO loop to deal with the fields */
        WN *top_label[MAX_DIM];
        WN *cont_label[MAX_DIM];
        WN *xstart, *xstep[MAX_DIM], *xend[MAX_DIM];
	WN *new_item;
	WN *arr_item;
	TY_IDX arr_ty;

	if (*icount > 1) {
	  /* icount should have a value of 0 which is incremented to 1
	  ** when you go through this path to handle an array of structures.
	  ** However, just make sure that all unprocessed I/O items are
	  ** handled here.
	  */
          word = 0;
          if (Target_Byte_Sex == LITTLE_ENDIAN) {
            io_header_ptr = (iolist_header_type *)&word;
            io_header_ptr->version = 1;
            io_header_ptr->iolfirst = (first_last == 2 || first_last == 3) ? 1 : 0;
            io_header_ptr->iollast = (first_last == 1 || first_last == 3) ? 1 : 0;
            io_header_ptr->icount = *icount;
            io_header_ptr->ioetsize = *iolist_size;
	  } else {
          word = ( (UINT64) 1 << 61) | /* version */
	       ( (UINT64) first_last << 32) | /* first_last flag*/
      ( (UINT64) *icount << 16) | /* icount */ 
	       ( (UINT64) *iolist_size );
	  }
          Set_TY_size(Ty_Table[*iolist_ty], *offset);
	  Set_FLD_last_field (last_field);
          Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn,
			stack_wn, *iolist_st, word);

	  *offset = 8;
	  *iolist_st = Get_IoStruct_ST ( block, FID_CRAY_IOLIST, FALSE);
	  *iolist_ty = fiostruct_ty[FID_CRAY_IOLIST];
	  Set_ST_type(*iolist_st, *iolist_ty);
	  *iolist_size = 8/Pointer_Size;
	  last_field = TY_fld (Ty_Table[*iolist_ty]);
	}
        *icount = 0;

        ARB_HANDLE arb_base = TY_arb(ty);
        ndims = TY_AR_ndims(ty);
	nkids = ndims * 2 + 1;
	kid0 = WN_Create( Pointer_Size == 4 ? OPC_U4ARRAY : OPC_U8ARRAY, nkids );
	for (arr_item = WN_kid0(item); WNOPR(arr_item) != OPR_LDA;)
	  arr_item = WN_kid0(arr_item);
	/*
	WN_element_size(kid0) = TY_size(TY_AR_etype(TY_pointed(WN_ty(WN_kid0(item)))));
	*/
	arr_ty = TY_pointed(WN_ty(arr_item));
	WN_element_size(kid0) = TY_size(TY_AR_etype(arr_ty));
	WN_kid0(kid0) = WN_COPY_Tree( arr_item );
	for (i = 0; i < ndims; i++) {
	  /* UARRAY kids */
	  idx_st[i] = Gen_Temp_Symbol( MTYPE_To_TY(MTYPE_I4), "struct_arr_idx");
	  ARB_HANDLE arb = arb_base[ndims-i-1];

	  /* beginning count of loop */
	  start = Get_ARB_WN(arb, ARB_LBOUND);

	  WN_kid(kid0, nkids-i-1) = WN_Sub( Int_Type,
		WN_Ldid(Int_Type, 0, idx_st[i], ST_type(idx_st[i]) ),
		start );
	  if (ARB_const_ubnd(arb) && ARB_const_lbnd(arb))
	    WN_kid(kid0, nkids-i-ndims-1) =
		WN_CreateIntconst( OPC_I4INTCONST,
			ARB_ubnd_val( arb ) - ARB_lbnd_val( arb ) + 1 );
	  else
	    WN_kid(kid0, nkids-i-ndims-1) = Get_ARB_WN(arb, ARB_UBOUND);

	}
	read_mode = form == FIO_CR_READ_UNFORMATTED
			|| form == FIO_CR_READ_FORMATTED
			|| form == FIO_CR_READ_NAMELIST;
	new_item = WN_CreateIoItem1( read_mode ? IOL_VAR : IOL_EXPR,
			kid0, (TY_IDX) 0 );
	WN_set_ty(new_item, TY_AR_etype(arr_ty));
	/* 
	** Create this dummy node to trick lower_f77_io_items which only
	** looks at the kids in the io tree and not the top node
	new_item = WN_CreateIoItem1( read_mode ? IOL_VAR : IOL_EXPR,
			new_item, NULL );
	WN_set_ty(new_item, TY_AR_etype(arr_ty));
	*/

	for (i = 0; i < ndims; i++) {
          top_label[i] = WN_CreateNewLabel();
          cont_label[i] = WN_CreateNewLabel();
	  ARB_HANDLE arb = arb_base[ndims-i-1];

	  /* beginning count of loop */
	  start = Get_ARB_WN(arb, ARB_LBOUND);;
	  end = Get_ARB_WN(arb, ARB_UBOUND);

	  step = WN_CreateIntconst( OPC_IntWord, 1 );

          ty = Be_Type_Tbl(Int_Type);
          ntype = mtype = TY_mtype(ty);
          if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
            ntype = MTYPE_I4;
          load_index = WN_Ldid ( mtype, 0, idx_st[i], ty );
          xstart = WN_Stid( mtype, 0, idx_st[i], ty, start );
                    
          xstep[i] = WN_Stid ( mtype, 0, idx_st[i], ty,
                     WN_CreateExp2( OPCODE_make_op( OPR_ADD, ntype, MTYPE_V ),
                                     WN_COPY_Tree( load_index ), step ));
          xend[i] = WN_LE ( ntype, load_index, end );
          WN_INSERT_BlockLast ( block, xstart );
          WN_INSERT_BlockLast ( block,
                WN_CreateGoto ((ST_IDX) NULL,WN_label_number(cont_label[i]) ));
          WN_INSERT_BlockLast ( block, top_label[i] );
	}

        lower_f77_record_items ( block, new_item, cilist_wn, stack_wn,
			      iostat1, iostat2, form, iolist_st, 
			      iolist_ty, iolist_size,
			      last_field, offset, icount,
			      NESTED_ITEM, TY_AR_etype(arr_ty), 0);

	for (i = ndims-1; i >= 0; i--) {
          WN_INSERT_BlockLast ( block, xstep[i] );
          WN_INSERT_BlockLast ( block, cont_label[i] );
          WN_INSERT_BlockLast( block, 
			WN_CreateTruebr( WN_label_number(top_label[i]), xend[i] ));
        }
	return(0);
      }
      /* Generate a DO loop for the implied-do */
      top_label = WN_CreateNewLabel();
      cont_label = WN_CreateNewLabel();
      WN_start(item) = extract_calls ( block, WN_start(item) );
      WN_end(item)   = extract_calls ( block, WN_end(item) );
      WN_step(item)  = extract_calls ( block, WN_step(item) );
      ty = ST_type(WN_st(WN_index(item)));
      if ( TY_kind(ty) != KIND_POINTER ) {
        ntype = mtype = TY_mtype(ty);
        if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
          ntype = MTYPE_I4;
        load_index = WN_Ldid ( mtype, WN_idname_offset(WN_index(item)),
			   WN_st(WN_index(item)), ty );
        start = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
		      WN_st(WN_index(item)), ty, WN_start(item) );
        step = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
		     WN_st(WN_index(item)), ty,
		     WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD, ntype,
						      MTYPE_V ),
				     WN_COPY_Tree ( load_index ),
				     WN_step(item) ));
      } else {
        ntype = mtype = TY_mtype(TY_pointed(ty));
        if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
          ntype = MTYPE_I4;
        load_index = WN_Iload ( mtype, 0, TY_pointed(ty),
			    WN_Ldid ( Pointer_type,
				      WN_idname_offset(WN_index(item)),
				      WN_st(WN_index(item)), ty ));
        start = WN_Istore ( mtype, 0, ty,
			WN_Ldid ( Pointer_type,
				  WN_idname_offset(WN_index(item)),
				  WN_st(WN_index(item)), ty ),
			WN_start(item) );
        step = WN_Istore ( mtype, 0, ty,
		       WN_Ldid ( Pointer_type,
				 WN_idname_offset(WN_index(item)),
				 WN_st(WN_index(item)), ty ),
		       WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD, ntype,
							MTYPE_V ),
				       WN_COPY_Tree ( load_index ),
			       WN_step(item) ));
      }
      if ( WN_operator(WN_step(item)) == OPR_INTCONST ||
           WN_operator(WN_step(item)) == OPR_CONST ) {
        if ( ( WN_operator(WN_step(item)) == OPR_INTCONST &&
	       WN_const_val(WN_step(item)) >= 0 ) ||
	     ( WN_operator(WN_step(item)) == OPR_CONST &&
       	       STC_val(WN_st(WN_step(item))).vals.ival.v0 >= 0 ) )
          end = WN_LE ( ntype, load_index, WN_end(item) );
        else
          end = WN_GE ( ntype, load_index, WN_end(item) );
      } else {
        pregst = MTYPE_To_PREG ( Boolean_type );
        pregnum = Create_Preg ( Boolean_type, "stoptemp");
        WN_INSERT_BlockLast ( block,
			      WN_StidIntoPreg ( Boolean_type, pregnum,
					        pregst,
				    WN_GE ( ntype,
					 WN_COPY_Tree ( WN_step(item) ),
					 WN_Zerocon ( ntype ))));
        end = WN_Select ( Boolean_type,
		      WN_LdidPreg ( Boolean_type, pregnum ),
		      WN_LE ( ntype, load_index, WN_end(item) ),
		      WN_GE ( ntype, WN_COPY_Tree (load_index),
			      WN_COPY_Tree (WN_end(item)) ));
      }
      WN_INSERT_BlockLast ( block, start );
      if (io_item == IOL_IMPLIED_DO)
	WN_INSERT_BlockLast ( block, 
		WN_CreateGoto ((ST_IDX) NULL,WN_label_number(cont_label) ));
      WN_INSERT_BlockLast ( block, top_label );

      Set_TY_size(Ty_Table[*iolist_ty], *offset);
      lower_f77_io_items ( block, item, cilist_wn, iostat1, iostat2,
		form, NESTED_ITEM, &impl_offset, 4, WN_kid_count(item) );

      WN_INSERT_BlockLast ( block, step );
      WN_INSERT_BlockLast ( block, cont_label );
      WN_INSERT_BlockLast( block, WN_CreateTruebr( WN_label_number(top_label), end ));
      return(0);
    }
  } else if (val_type == 4) {
    /* Calvin TODO */
    fprintf( stderr, "valtype = %d not done\n", val_type );
    abort();
  } else if (val_type == 5) {
    /* Calvin TODO */
    fprintf( stderr, "valtype = %d not done\n", val_type );
    abort();
  }
  return (ioentsize);
}


static WN *
Convert_Iol_Item( WN *item, TY_IDX fty )
{
  /* Convert a record item into a field item */
  WN *wn;
  WN *kid0;
  TY_IDX ety;

    if (TY_kind( fty ) == KIND_ARRAY) {
      /* distinguish character IOL_CHAR & IOL_ARRAY */
      /* Do not need to create IOL_LOGICAL */
      /* Array of structure not handled here, only simple I/O item */
      ety = TY_AR_etype(fty);
      if (TY_kind( ety ) == KIND_ARRAY) {
	/* while (TY_kind(ety) == KIND_ARRAY) */
	  ety = TY_AR_etype(ety);
        if (TY_is_character( ety )) {
	  /* IOL_CHAR_ARRAY */
	  ety = TY_AR_etype(fty);
	  wn  = WN_CreateIoItem3 ( IOL_CHAR_ARRAY, 
		WN_COPY_Tree( WN_kid0( item ) ), 
		WN_CreateIntconst( OPC_I4INTCONST, TY_size(ety)),
		WN_CreateIntconst ( OPC_I4INTCONST, TY_size(fty)/TY_size(ety)), 
		ety );
	  fty = ety;
	} else {
	  /* IOL_ARRAY */
	  wn  = WN_CreateIoItem2 ( IOL_ARRAY, WN_COPY_Tree( WN_kid0( item ) ),
		WN_CreateIntconst( OPC_I4INTCONST, TY_size(fty)), fty );
	}
      } else if (TY_mtype( ety ) == MTYPE_U1) {
	  /* IOL_CHAR */
	  wn  = WN_CreateIoItem2 ( IOL_CHAR, WN_COPY_Tree( WN_kid0( item ) ),
		WN_CreateIntconst ( OPC_I4INTCONST, TY_size(fty)), fty );
      }
      else {
	/* IOL_ARRAY */
	wn  = WN_CreateIoItem2 ( IOL_ARRAY, WN_COPY_Tree( WN_kid0( item ) ),
		WN_CreateIntconst( OPC_I4INTCONST, TY_size(fty)), fty );
      }
    } else { /* not KIND_ARRAY */
      /* scalar I/O item */
      if ( WN_io_item(item) == IOL_EXPR)
  	wn = WN_CreateIoItem1( IOL_EXPR, WN_COPY_Tree( WN_kid0(item) ), 
		fty );
      else
	wn = WN_CreateIoItem1( IOL_VAR, WN_COPY_Tree( WN_kid0(item) ), fty );
    }

  kid0 = WN_kid0(wn);
  while (WNOPR(kid0) != OPR_LDA) {
    kid0 = WN_kid0(kid0);
  }
  WN_set_ty(kid0, TY_pointer(fty));
  /*WN_set_ty(wn, ty);*/
  return( wn );
}


static INT32
Create_Field_Entry ( WN *block, WN * item, WN *cilist_wn, WN *stack_wn,
		  WN *iostat1, WN *iostat2,
		  FIOOPER form, ST **iolist_st, 
		  TY_IDX *iolist_ty, INT32 *iolist_size,
		  FLD_HANDLE& last_field, INT32 *offset, INT32 *icount,
		  INT32 nested, FLD_HANDLE fld, INT64 foffset)
/* 
** This function deals with a single field in a record,
**
** bloc		: current block
** item		: I/O item to be added
** cilist_wn	: current cilist
** iostat1	: 
** iostat2	: 
** form		: formatting value/flag
** iolist_st	: current iolist
** last_field	: last field in the I/O list	(updated)
** offset	: entry offset in the I/O list	(updated)
** nested:	: whether the item is inside an implieddo_entry
		: (i.e. converted to dope vector when appropriate)
		: or inside a do-loop generated from an implied-do/array.
** fld		: the field in the record
** foffset	: the offset of the field in the record
*/
{
  TY_IDX ty;
  WN *wn1, *kid0;
  INT32 ioentsize = 0;

  kid0 = WN_kid0(item);
  ty = FLD_type( fld );

  switch (TY_kind(ty)) {

    case KIND_SCALAR:
    case KIND_POINTER:
      (void) Make_Pointer_Type ( ty, FALSE );
      wn1 = Convert_Iol_Item(item, ty);
      kid0 = WN_kid0(wn1);
      while (WNOPR(kid0) != OPR_LDA) {
	kid0 = WN_kid0(kid0);
      }
      WN_lda_offset(kid0) += foffset;
      ioentsize = Create_Io_Entry( block, wn1, cilist_wn, stack_wn,
			iostat1, iostat2, form, iolist_st, 
			iolist_ty, iolist_size,
			last_field, offset, icount, nested, FALSE);
      break;

    case KIND_ARRAY:
      if (TY_kind(TY_AR_etype(ty)) == KIND_STRUCT) {
        (void) Make_Pointer_Type ( ty, FALSE );
        wn1 = Convert_Iol_Item(item, ty);
        kid0 = WN_kid0(wn1);
        while (WNOPR(kid0) != OPR_LDA) {
	  kid0 = WN_kid0(kid0);
        }
        WN_lda_offset(kid0) += foffset;
        ioentsize = Create_Io_Entry( block, wn1, cilist_wn, stack_wn,
			iostat1, iostat2, form, iolist_st, 
			iolist_ty, iolist_size,
			last_field, offset, icount, nested, TRUE);
      }
      else {
        (void) Make_Pointer_Type ( ty, FALSE );
        wn1 = Convert_Iol_Item(item, ty);
        kid0 = WN_kid0(wn1);
        while (WNOPR(kid0) != OPR_LDA) {
	  kid0 = WN_kid0(kid0);
        }
        WN_lda_offset(kid0) += foffset;
        ioentsize = Create_Io_Entry( block, wn1, cilist_wn, stack_wn,
			iostat1, iostat2, form, iolist_st, 
			iolist_ty, iolist_size,
			last_field, offset, icount, nested, FALSE);
      }
      break;

    case KIND_STRUCT:
       lower_f77_record_items ( block, item, cilist_wn, stack_wn,
			      iostat1, iostat2, form, iolist_st, 
			      iolist_ty, iolist_size,
			      last_field, offset, icount,
			      nested, ty, foffset);
      break;
    default:
      Fail_FmtAssertion("unexpected type (%s) in record I/O processing",
			MTYPE_name(TY_mtype(ty)));
  }
  return( ioentsize );
}


/* ====================================================================
 *
 * Perform lowering of record io item for the Cray I/O library.  This
 * is done for F77 only as F90 structures are splitted by the FE.
 *
 * ==================================================================== */

static void
lower_f77_record_items ( WN * block, WN *rec, WN *cilist_wn, WN *stack_wn,
			      WN *iostat1, WN *iostat2,
			      FIOOPER form, ST **iolist_st, 
			      TY_IDX * iolist_ty, INT32 *iolist_size,
			      FLD_HANDLE& last_field, INT32 *offset, INT32 *icount,
			      INT32 nested, TY_IDX rty, INT64 roffset)
/*
** This function deals with a single record.  It makes calls to the
** I/O runtime library and so will not add to the iolist.  Also
** the iolist, before calling lower_f77_record_items() must be cleared,
** with an appropriate call to the I/O runtime library.
*/
{
  TY_IDX ty;
  INT64 foffset;
  UINT64 word;
  iolist_header_type *iolist_header_ptr;

  Is_True (TY_kind(rty) == KIND_STRUCT,
	   ("non record type passed to lower_f77_record_items"));

  /* For unformatted I/O treat the whole record as an array of bytes */
  // Actually, we don't want to do this, so the code below should be ignored
  // Richard Shapiro 8/24/98
  //  if (form == FIO_CR_READ_UNFORMATTED || form == FIO_CR_WRITE_UNFORMATTED) {
  //  TY_IDX arr_ty;
  //  WN *wn;

  //  arr_ty = Make_Simple_Array_Type("unformatted_struct_type", 
  //		TY_size(rty), Be_Type_Tbl(MTYPE_I1));
  //  wn = Convert_Iol_Item(rec, arr_ty);
  //  Create_Io_Entry( block, wn, cilist_wn, stack_wn,
  //			iostat1, iostat2, form, iolist_st, 
  //			iolist_ty, iolist_size,
  //			last_field, offset, icount, nested, FALSE);
  // return;
  // }

  /*  Process all fields in the record.  */

  FLD_ITER fld_iter = Make_fld_iter (TY_fld(Ty_Table[rty]));

  do {
  
    FLD_HANDLE fld (fld_iter);

    ty = FLD_type(fld);
    foffset = roffset + FLD_ofst(fld);

    switch (TY_kind(ty)) {

      case KIND_ARRAY:
        if (TY_kind(TY_AR_etype(ty)) == KIND_STRUCT) {
	  /* 
	  ** Since the array of structures needs to have an implied-do
	  ** loop generated for it all scalar variables accumulated in
	  ** the iolist so far have to be flushed out.
	  ** Since all I/O items containing an array of records
	  ** whether nested within a record or not, are processed
	  ** independently with the need_loop flag set we can assume
	  ** that there is only a single iolist which is not nested
	  ** here (i.e. no implied-do entry preceding this).
	  */
	  if (*icount) {
            word = 0;
	    if (Target_Byte_Sex == LITTLE_ENDIAN) {
              iolist_header_ptr = (iolist_header_type *)&word;
              iolist_header_ptr->version = 1;
              iolist_header_ptr->iolfirst = (first_last == 2 || first_last == 3) ? 1 : 0;
              iolist_header_ptr->iollast = (first_last == 1 || first_last == 3) ? 1 : 0;
              iolist_header_ptr->icount = *icount;
              iolist_header_ptr->ioetsize = *iolist_size;
	    } else {
            word = ( (UINT64) 1 << 61) | /* version */
	       ( (UINT64) first_last << 32) | /* first_last flag*/
	       ( (UINT64) *icount << 16) | /* icount */ 
	       ( (UINT64) *iolist_size );
	    }
            Set_TY_size(*iolist_ty, *offset);
	    Set_FLD_last_field (last_field);
            Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn,
			stack_wn, *iolist_st, word);

	    *icount = 0;
	    *offset = 8;
	    *iolist_st = Get_IoStruct_ST ( block, FID_CRAY_IOLIST, FALSE);
	    *iolist_ty = fiostruct_ty[FID_CRAY_IOLIST];
	    Set_ST_type(*iolist_st, *iolist_ty);
	    *iolist_size = 8/Pointer_Size;
	    last_field = TY_fld (Ty_Table[*iolist_ty]);
	  }
	  else if (first_last) {
	    Create_Null_Call( block, form, cilist_wn, iostat1, iostat2,
			stack_wn);
	  }
	  Create_Field_Entry( block, rec, cilist_wn, stack_wn,
				iostat1, iostat2, form,
				iolist_st, iolist_ty, iolist_size,
				last_field, offset, icount,
				nested, fld, foffset );
	  break;
	}
      case KIND_SCALAR:
      case KIND_POINTER:

	*iolist_size += Create_Field_Entry( block, rec, cilist_wn, stack_wn,
				iostat1, iostat2, form,
				iolist_st, iolist_ty, iolist_size,
				last_field, offset, icount,
				nested, fld, foffset );
	break;

      case KIND_STRUCT:
	lower_f77_record_items ( block, rec, cilist_wn,  stack_wn,
			iostat1, iostat2, form, iolist_st, 
			iolist_ty, iolist_size,
			last_field, offset, icount, nested, ty, foffset );
	break;

      case KIND_VOID:
	/* Union type, ignore */
	break;

      default:
	Fail_FmtAssertion("unexpected type (%s) in record I/O processing",
			  MTYPE_name(TY_mtype(ty)));

    }
  } while (! FLD_last_field (fld_iter++));
  
  if (*icount) {
    word = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      iolist_header_ptr = (iolist_header_type *)&word;
      iolist_header_ptr->version = 1;
      iolist_header_ptr->iolfirst = (first_last == 2 || first_last == 3) ? 1 : 0;
      iolist_header_ptr->iollast = (first_last == 1 || first_last == 3) ? 1 : 0;
      iolist_header_ptr->icount = *icount;
      iolist_header_ptr->ioetsize = *iolist_size;
    } else {
    word = ( (UINT64) 1 << 61) | /* version */
	       ( (UINT64) first_last << 32) | /* first_last flag*/
	       ( (UINT64) *icount << 16) | /* icount */ 
	       ( (UINT64) *iolist_size );
    }
    Set_TY_size(*iolist_ty, *offset);
    Set_FLD_last_field (last_field);
    Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn,
			stack_wn, *iolist_st, word);

    *icount = 0;
    *offset = 8;
    *iolist_st = Get_IoStruct_ST ( block, FID_CRAY_IOLIST, FALSE);
    *iolist_ty = fiostruct_ty[FID_CRAY_IOLIST];
    *iolist_size = 8/Pointer_Size;
    last_field = TY_fld (Ty_Table[ST_type(*iolist_st)]);
  }
}

static INT32 
lower_f77_io_items ( WN * block, WN * tree, WN *cilist_wn, 
		     WN *iostat1, WN *iostat2,
		     FIOOPER form, INT32 nested, 
		     INT32 *offset,
		     INT32 kid_first, INT32 kid_last )
{
  INT32 icount, i, itm;
  INT32 ioentsize;
  INT32 header_offset;
  UINT64 word;
  static ST *iolist_st;
  static TY_IDX iolist_ty;
  static FLD_HANDLE last_field;
  INT32 iolistsize;
  WN *item;
  static WN *stack_wn;
  INT32 iomode;
  INT32 iend;
  BOOL last_loop = FALSE;	/* last I/O item is expanded into a loop */
  iolist_header_type *iolist_header_ptr;
#define WRITE_STMT 0
#define READ_STMT  1
  
#ifdef _IO_DEBUG
if (!nested) {
fprintf(stderr, "\n\n\n" );
dump_tree(tree);
}
#endif /*_IO_DEBUG */
  if (form == FIO_CR_READ_FORMATTED || form == FIO_CR_READ_UNFORMATTED
      || form == FIO_CR_READ_NAMELIST)
    iomode = READ_STMT;
  else
    iomode = WRITE_STMT;

  if (nested == FALSE) {
    first_last = 2;	/* first last */
    iolist_st = NULL;
    iolist_ty = (TY_IDX) 0;
    last_field = FLD_HANDLE();
  }

  for (i = 0; i <= MAX_DIM; i++) {
     if (!dope_vector_ty[ i ]) {
	make_dope_vector_ty(i);
     }
  }

  if (nested != NESTED_DOPE) {
    cwh_io_unmark();
    if (stack_ty == (TY_IDX) 0)
      stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));
    if (Current_pu != cray_iolist_current_pu) {
      cray_iolist_current_pu = Current_pu;
      stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
    }
    stack_wn = WN_CreateLda (opc_lda, 0,
			Make_Pointer_Type (stack_ty, FALSE), stack_st);
    io_set_addr_passed_flag(stack_st);

  } else {
    /* Starting a "new" iolist for the implied-do and append it to
    ** the old iolist
    */

    FLD_HANDLE fld =  New_FLD ();
    FLD_Init (fld, Save_Str("implied_do_iolist_header"),
              Be_Type_Tbl (MTYPE_U8), *offset);
    last_field = fld;
  }

  /*  Process all of the I/O items.  */
  for (itm=kid_first; itm<kid_last; ) {
    if (last_loop)
      /* 
      ** If last_loop is set then the previously-created iolist has not
      ** been used and we can simply re-use it */
      *offset = 0;
    else if (nested != NESTED_DOPE && !last_loop) {
      /* start from a fresh io_list each time */
      iolist_st = Get_IoStruct_ST ( block, FID_CRAY_IOLIST, FALSE);
      iolist_ty = fiostruct_ty[FID_CRAY_IOLIST];
      last_field = TY_flist (Ty_Table [iolist_ty]);
      /* reset offset to zero */
      *offset = 0;
    }

    /* scan the list to see how many of the I/O items can be bundled
    ** together into one call */
    header_offset = *offset;
    *offset += 8;
    iolistsize = 8/Pointer_Size;
    icount = 0;
    if (nested != NESTED_DOPE) {
      iend = itm;
      while (iend < kid_last) {
        item = WN_kid(tree,iend);
        if (cwh_io_analyse_io_item( item, NULL, iomode ))
	  break;;
        iend++;
      }
    }
    else {
      iend = kid_last;
    }

    if (itm == iend) {
      /* can only process a single I/O item */
        /* If the first item in the I/O list is an implied-do that
        ** needs to be turned into a DO loop then we have to generate an
        ** null call as the first call to the I/O lib.
        */
        if (first_last) {
	  Create_Null_Call( block, form, cilist_wn, iostat1, iostat2,
			stack_wn);
        }
        item = WN_kid(tree,itm);
        /* Create an entry for the implied-do with the need_loop flag
        ** set to TRUE.  Note that this call to Create_Io_Entry will
        ** need to make as many calls to the I/O runtime library
        ** within the DO-loop as needed.  
        */
        ioentsize = Create_Io_Entry( block, item, cilist_wn,  stack_wn,
				iostat1, iostat2, form, &iolist_st, 
				&iolist_ty, &iolistsize,
				last_field, offset, &icount, nested, TRUE);
        itm++;
	if (!ioentsize) {
	  /* when the I/O item has to be expanded into a loop and all the
	  ** I/O calls have been made.  Same thing with structure.
	  */
	  if (!nested) {
            last_loop = TRUE;
            cwh_stab_free_auxst();
	  }
          continue;
	}
	else {
	  /* regular single item */
          iolistsize += ioentsize;
          last_loop = FALSE;
	}
    } else {
    /* All the items from itm to iend-1 can be done in one call */
      last_loop = FALSE;
      while (itm < iend) {
        item = WN_kid(tree,itm);
        ioentsize = Create_Io_Entry( block, item, cilist_wn,  stack_wn,
				iostat1, iostat2, form, &iolist_st, 
				&iolist_ty, &iolistsize,
				last_field, offset, &icount, nested, FALSE);
	  
        iolistsize += ioentsize;
        itm++;
	if (!ioentsize && !nested)
	  last_loop = TRUE;
	else
	  last_loop = FALSE;
      }
    }
    /* 
    ** If the last item is a record which has already had its I/O calls made
    ** then process the next item in the list
    */
    if (last_loop) {
      if (!nested)
        cwh_stab_free_auxst();
      continue;
    }
    if (!nested) {
      /* 
      ** Generate a call to the I/O runtime routine if this is not a
      ** implied-loop dope vector
      */
      if (icount) {
        if (iend >= kid_last || itm >= kid_last)
          first_last |= 1;
        word = 0;
	if (Target_Byte_Sex == LITTLE_ENDIAN) {
          iolist_header_ptr = (iolist_header_type *)&word;
          iolist_header_ptr->version = 1;
          iolist_header_ptr->iolfirst = (first_last == 2 || first_last == 3) ? 1 : 0;
          iolist_header_ptr->iollast = (first_last == 1 || first_last == 3) ? 1 : 0;
          iolist_header_ptr->icount = icount;
          iolist_header_ptr->ioetsize = iolistsize;
	} else {
        word = ( (UINT64) 1 << 61) | /* version */
               ( (UINT64) first_last << 32) | /* first_last flag*/
	       ( (UINT64) icount << 16) | /* icount */ 
	       ( (UINT64) iolistsize );
	}
        Set_TY_size (Ty_Table [iolist_ty], *offset);
	Set_FLD_last_field (last_field);
        Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn, 
			stack_wn, iolist_st, word);
      }
      cwh_io_unmark();
    } else if (nested == NESTED_ITEM) {
      /* 
      ** Generate a call to the I/O runtime for the I/O items in the
      ** implied-do list which has to be implemented as a DO loop.
      */
      word = 0;
      if (Target_Byte_Sex == LITTLE_ENDIAN) {
        iolist_header_ptr = (iolist_header_type *)&word;
        iolist_header_ptr->version = 1;
        iolist_header_ptr->icount = icount;
        iolist_header_ptr->ioetsize = iolistsize;
      } else {
      word = ( (UINT64) 1 << 61) | /* version */
	       ( (UINT64) icount << 16) | /* icount */ 
	       ( (UINT64) iolistsize );
      }

      Set_TY_size (Ty_Table [iolist_ty], *offset);
      Set_FLD_last_field (last_field);

      Make_Cray_Io_Call( block, form, iostat1, iostat2, cilist_wn,
			stack_wn, iolist_st, word);
    } else if (nested == NESTED_DOPE) {
      word = 0;
      if (Target_Byte_Sex == LITTLE_ENDIAN) {
        iolist_header_ptr = (iolist_header_type *)&word;
        iolist_header_ptr->version = 1;
        iolist_header_ptr->icount = icount;
        iolist_header_ptr->ioetsize = iolistsize;
      } else {
      word = ( (UINT64) 1 << 61) | /* version */
	       ( (UINT64) icount << 16) | /* icount */ 
	       ( (UINT64) iolistsize );
      }
      Gen_Iolist_PutFieldConst( block, iolist_st, header_offset, 
				MTYPE_U8, word);
    }
    /* After each group of I/O items is done, reset all the symbol
    ** dependencies 
    */
    if (!nested)
      cwh_stab_free_auxst();
  }  /* FOR loop: all I/O items in list */

  if (last_loop || kid_first >= kid_last) {
    /* The last item in the I/O loop is an implied-do loop that needs
    ** to have a DO loop generated.  In this case we need a NULL call
    ** with the first/last flag set to last.  Same thing if the I/O
    ** statement is a NULL I/O statement no no items in the iolist.
    */
    first_last |= 1;
    Create_Null_Call( block, form, cilist_wn, iostat1, iostat2, 
			stack_wn );
  }
  if (nested != NESTED_DOPE)
    cwh_io_unmark();
  return( iolistsize );
}


static void lower_cray_io_items ( WN * block, WN * tree, INT32 kid_first, 
				  INT32 kid_last, BOOL needs_new_iolist_table, 
				  INT32 *word_count, INT32 flflag, 

				  WN *cilist_wn, FIOOPER form)
{
  static ST *iolist_st;
  static TY_IDX iolist_ty;
  static FLD_HANDLE last_field;
  static INT32 offset;
  static INT32 iolistsize;
  TY_IDX ty;
  WN *item;
  UINT64 word;
  INT32 icount;
  INT32 i;
  INT32 j;
  INT32 ioentsize;
  INT32 valtype;
  INT32 bound_check;
  INT32 indflag;
  IOITEM io_item;
  WN *kid0, *char_len;
  WN *wn;
  WN *dope_kid;
  INT32 save_impdo_word1_offset;
  INT32 save_iotable_word1_offset;
  WN *iolist_wn = NULL;
  UINT64 cray_type;
  WN *stack_wn = NULL;
  WN *dummy;
  ioentry_header_type *ioentry_header_ptr;
  ioarray_entry_type *ioarray_entry_ptr;
  iolist_header_type *iolist_header_ptr;
  
  if (stack_ty == (TY_IDX) 0)
     stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));
     
  if (Current_pu != cray_iolist_current_pu) {
     cray_iolist_current_pu = Current_pu;
     stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
     io_set_addr_passed_flag(stack_st);
     container_block_for_iolists = NULL;
     num_iolists = 0;
  }

  stack_wn = WN_CreateLda (opc_lda, 0, 
			   Make_Pointer_Type (ST_type(stack_st), FALSE),
                           stack_st);

  if (needs_new_iolist_table) {
    iolist_st = Get_IoStruct_ST ( block, FID_CRAY_IOLIST, FALSE);
    iolist_ty = fiostruct_ty[FID_CRAY_IOLIST];
    save_iotable_word1_offset = 0;
    offset = 8;
    iolistsize = 8/Pointer_Size;
  } else {
    save_iotable_word1_offset = offset;
    FLD_HANDLE field = New_FLD ();
    FLD_Init (field, Save_Str("cray_iol_head"), 
	      Be_Type_Tbl (MTYPE_U8), offset);
    offset += 8;
    iolistsize += 8/Pointer_Size;
  }
    
  *word_count = 8/Pointer_Size;

  icount = kid_last - kid_first;

  /*  Process all of the I/O items.  */

  for (i=kid_first; i<kid_last; i++) {

    item = WN_kid(tree,i);
    io_item = (IOITEM) WN_intrinsic(item);
    switch (io_item) {

       case IOL_EXPR:
	  {
	     kid0 = WN_kid0(item);
	     ty = WN_ty(item);

             if (TY_kind(ty) != KIND_STRUCT && 
                 TY_kind(ty) == KIND_SCALAR &&
                 TY_kind(ty) == KIND_POINTER)
                Fail_FmtAssertion(
                   "unexpected machine type (%s) for IOL_EXPR in IO Processing",
                   MTYPE_name(TY_mtype(ty)));

             cray_type = WN_const_val(WN_kid1(item));

             valtype = 1; 
             if (Pointer_Size == 4)
                ioentsize = 6;
             else
                ioentsize = 4;
             
	     FLD_HANDLE fld = New_FLD ();
	     FLD_Init (fld, Save_Str("scalar_word1"), 
		       Be_Type_Tbl(MTYPE_U8), offset);

             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioentry_header_ptr = (ioentry_header_type *)&word;
               ioentry_header_ptr->valtype = valtype;
               ioentry_header_ptr->ioentsize = ioentsize;
	     } else {
               word = ( (UINT64) valtype << 56) | 
	              ( (UINT64) ioentsize );
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word );
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init (fld, Save_Str("scalar_word2"),
		       Be_Type_Tbl (MTYPE_U8), offset);
            
	     word =  (UINT64) cray_type;
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word);
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init (fld, Save_Str("addr_of_scalar"),
		       Be_Type_Tbl (Pointer_type), offset);

	     wn = create_pointer_to_node (block, kid0, ty, FALSE);
	     Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    wn );
             offset += fcd_size;

             iolistsize += ioentsize;
	     *word_count += ioentsize;
	     break;
         }
       
       case IOL_VAR:
	 {
	     kid0 = WN_kid0(item);
	     ty = WN_ty(item);

             if (TY_kind(ty) != KIND_STRUCT && 
                 TY_kind(ty) == KIND_SCALAR &&
                 TY_kind(ty) == KIND_POINTER)
                Fail_FmtAssertion(
                   "unexpected machine type (%s) for IOL_EXPR in IO Processing",
                   MTYPE_name(TY_mtype(ty)));

             cray_type = WN_const_val(WN_kid1(item));

             valtype = 1; 
             if (Pointer_Size == 4)
                ioentsize = 6;
             else
                ioentsize = 4;
             
	     FLD_HANDLE fld = New_FLD ();
	     FLD_Init(fld, Save_Str("scalar_word1"),
		      Be_Type_Tbl (MTYPE_U8), offset);

             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioentry_header_ptr = (ioentry_header_type *)&word;
               ioentry_header_ptr->valtype = valtype;
               ioentry_header_ptr->ioentsize = ioentsize;
	     } else {
               word = ( (UINT64) valtype << 56) | 
	              ( (UINT64) ioentsize );
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word );
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("scalar_word2"),
		      Be_Type_Tbl (MTYPE_U8), offset);

	     word =  (UINT64) cray_type;
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word);
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("addr_of_scalar"),
		      Be_Type_Tbl (Pointer_type), offset);

	     Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    kid0 );
             offset += fcd_size;

             iolistsize += ioentsize;
	     *word_count += ioentsize;
	     break;
         }

       case IOL_CHAR:
         {
	     kid0 = WN_kid0(item);
	     char_len = WN_kid2(item);
	     valtype = 1;
             cray_type = WN_const_val(WN_kid1(item));

             if (Pointer_Size == 4)
                ioentsize = 6;
             else
                ioentsize = 4;

	     FLD_HANDLE fld = New_FLD ();
	     FLD_Init(fld, Save_Str("char_word1"),
		      Be_Type_Tbl (MTYPE_U8), offset);

             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioentry_header_ptr = (ioentry_header_type *)&word;
               ioentry_header_ptr->valtype = valtype;
               ioentry_header_ptr->ioentsize = ioentsize;
	     } else {
               word = ( (UINT64) valtype << 56) |
                      ( (UINT64) ioentsize );
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word);
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("char_word2"),
		      Be_Type_Tbl (MTYPE_U8), offset);

             word = (UINT64) cray_type;
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word);
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("fcd_item_addr"),
		      Be_Type_Tbl (Pointer_type), offset);

             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    kid0 );
             offset += Pointer_Size;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("fcd_item_len"),
		      Be_Type_Tbl (Pointer_type), offset);

             Gen_Iolist_PutFieldWN ( block, iolist_st, offset, Pointer_type,
				     char_len );
             offset += Pointer_Size;

             iolistsize += ioentsize;
	     *word_count += ioentsize;
	     break;
        }

       case IOL_DOPE:  
	 {

             kid0 = WN_kid0(item);
	     bound_check = 0;
             valtype = 2;
             if (WN_kid_count(item) == 2) {
                indflag = 0;
                if (Pointer_Size == 4)
                   ioentsize = 5;
                else 
                   ioentsize = 3;
             } else {
                indflag = 1;
                if (Pointer_Size == 4)
                   ioentsize = 5 + WN_kid_count(item) - 2;
                else
                   ioentsize = 3 + WN_kid_count(item) - 2;
             }

	     /* The second kid of IOL_DOPE is just the address of the
		array from which the dope vector was created; this 
		needs to be passed as a dummy arg on the call */

             dummy = WN_kid1(item);
             Add_To_Dummy_List(dummy);

	     FLD_HANDLE fld = New_FLD ();
	     FLD_Init(fld, Save_Str("dope_word1"),
		      Be_Type_Tbl (MTYPE_U8), offset);
	      
             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioentry_header_ptr = (ioentry_header_type *)&word;
               ioentry_header_ptr->valtype = valtype;
               ioentry_header_ptr->ioentsize = ioentsize;
	     } else {
               word = ( (UINT64) valtype << 56) |
                      ( (UINT64) ioentsize );
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word );
             offset += 8;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("dope_addr"),
		      Be_Type_Tbl (Pointer_type), offset);

             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    kid0 );
             offset += Pointer_Size;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("dope_word3"),
		      Be_Type_Tbl (MTYPE_U8), offset);

             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioarray_entry_ptr = (ioarray_entry_type *)&word;
               ioarray_entry_ptr->indflag = indflag;
               ioarray_entry_ptr->boundchk = bound_check;
	     } else {
               word = ( (UINT64) indflag << 63) |
                      ( (UINT64) bound_check << 62);
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st, offset, MTYPE_U8,
				       word );
             offset += 8;

             if (indflag == 1) {
	       char str[64];
	       strcpy(str,"dope_dim");
               for (j=2; j<WN_kid_count(item); j++) {
		   sprintf(&str[8], "%d", j);
                   dope_kid = WN_kid(item,j);
                   if ((WN_operator(dope_kid) == OPR_INTCONST) && 
					  (WN_const_val(dope_kid) == 0)) {
                      fld = New_FLD ();
		      FLD_Init(fld, Save_Str(str),
			       Be_Type_Tbl (Pointer_type), offset);
		      Gen_Iolist_PutFieldConst( block, iolist_st, offset,
						Pointer_type, 0);
		      offset += Pointer_Size; /* TODO: check on this */
                   } else {
		      fld = New_FLD ();
		      FLD_Init(fld, Save_Str(str),
			       Be_Type_Tbl (Pointer_type), offset);
                      wn = create_pointer_to_node (block, dope_kid,
						   (TY_IDX) 0, TRUE);
                      Gen_Iolist_PutAddrWN ( block, iolist_st, offset,
					     Pointer_type, wn );
		      offset += Pointer_Size;
                   }
               }
             }
             iolistsize += ioentsize;
             *word_count += ioentsize;
	     break;
         }

       case IOL_IMPLIED_DO:
	 {

	     valtype = 3;
	     save_impdo_word1_offset = offset;

             FLD_HANDLE fld = New_FLD ();
	     FLD_Init(fld, Save_Str("imp_do_word1"),
		      Be_Type_Tbl (MTYPE_U8), offset);
             /* This field will be filled in later */
             offset += 8;
 
	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("imp_do_loop_var"),
		      Be_Type_Tbl (Pointer_type), offset);

	     wn = WN_CreateLda (
                         opc_lda, 0, 
                         Make_Pointer_Type(ST_type(WN_st(WN_index(item))),
                                           FALSE),
                         WN_st(WN_index(item)));

             io_set_addr_passed_flag(WN_st(WN_index(item)));
             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    wn );
             offset += Pointer_Size;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("imp_do_start"),
		      Be_Type_Tbl (Pointer_type), offset);

             wn = create_pointer_to_node (block, WN_start(item),
					  (TY_IDX) 0, TRUE);
             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    wn );
             offset += Pointer_Size;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("imp_do_end"),
		      Be_Type_Tbl (Pointer_type), offset);

             wn = create_pointer_to_node (block, WN_end(item), (TY_IDX) 0,
					  TRUE);
             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    wn );
             offset += Pointer_Size;

	     fld = New_FLD ();
	     FLD_Init(fld, Save_Str("imp_do_incr"),
		      Be_Type_Tbl (Pointer_type), offset);

             wn = create_pointer_to_node (block, WN_step(item),
					  (TY_IDX) 0, TRUE);
             Gen_Iolist_PutAddrWN ( block, iolist_st, offset, Pointer_type,
				    wn );
             offset += Pointer_Size;

             ioentsize = 0;
             lower_cray_io_items(block, item, 4, WN_kid_count(item), 
				 FALSE, &ioentsize, flflag, cilist_wn, form); 
             if (Pointer_Size == 4) {
                ioentsize = ioentsize + 6;
                iolistsize += 6;
             } else {
                ioentsize = ioentsize + 5;
                iolistsize += 5;
             }
             *word_count += ioentsize;

             word = 0;
	     if (Target_Byte_Sex == LITTLE_ENDIAN) {
               ioentry_header_ptr = (ioentry_header_type *)&word;
               ioentry_header_ptr->valtype = valtype;
               ioentry_header_ptr->ioentsize = ioentsize;
	     } else {
               word = ( (UINT64) valtype << 56) |
                      ( (UINT64) ioentsize );
	     }
             Gen_Iolist_PutFieldConst( block, iolist_st,
				       save_impdo_word1_offset, MTYPE_U8, word);
             break;
        }

       default:
	     break;

     }
  }

  if (needs_new_iolist_table) {
    word = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      iolist_header_ptr = (iolist_header_type *)&word;
      iolist_header_ptr->version = 1;
      iolist_header_ptr->iolfirst = (flflag == 2 || flflag == 3) ? 1 : 0;
      iolist_header_ptr->iollast = (flflag == 1 || flflag == 3) ? 1 : 0;
      iolist_header_ptr->icount = icount;
      iolist_header_ptr->ioetsize = iolistsize;
    } else {
    word = ( (UINT64) 1 << 61) | /* version */
           ( (UINT64) flflag << 32) | /* first_last flag*/
	   ( (UINT64) icount << 16) | /* icount */ 
	   ( (UINT64) iolistsize );
    }
    Set_TY_size (Ty_Table[iolist_ty], offset);
    Gen_Iolist_PutFieldConst (block, iolist_st, save_iotable_word1_offset,
			      MTYPE_U8, word);
    Enter_TY(iolist_ty);
    if (container_block_for_iolists == NULL) {
       container_block_for_iolists = iolist_st;
       num_iolists++;
    } else {
       if (num_iolists == iolist_reuse_limit) {
	  num_iolists = 1;
	  container_block_for_iolists = iolist_st;
       } else { 
         /* Force all previous iolists in this PU and the current iolist to 
          * use the same memory.
	  */ 
          St_Block_Union(container_block_for_iolists, iolist_st);
	  num_iolists++;
       }
    }
    iolist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(iolist_st)),
			      iolist_st);

    if (PU_has_region(Get_Current_PU())) {
      /* We are in a region; Stuff block2 after each call instead of
	 stuffing block1 after all (but last) call and generating a
	 branch from it to block2; the reason we do it this way is
	 because generation of block1 requires the IO lowerer to
	 generate a new label, which is not exposed to region processing.
       */
      GEN_IO_CALL_3 ( block, form, NULL, cr_iostat2, cilist_wn, 
		      iolist_wn, stack_wn);
    } else {
       if (LAST_CALL(flflag)) {
         GEN_IO_CALL_3 ( block, form, NULL, cr_iostat2, cilist_wn, 
		      iolist_wn, stack_wn);
       } else {
         GEN_IO_CALL_3 ( block, form, cr_iostat1, NULL, cilist_wn, 
		      iolist_wn, stack_wn);
       }
    }

  } else {
    word = 0;
    if (Target_Byte_Sex == LITTLE_ENDIAN) {
      iolist_header_ptr = (iolist_header_type *)&word;
      iolist_header_ptr->version = 1;
      iolist_header_ptr->iolfirst = 0;
      iolist_header_ptr->iollast = 0;
      iolist_header_ptr->icount = icount;
      iolist_header_ptr->ioetsize = *word_count;
    } else {
      word = ( (UINT64) 1 << 61) | /* version */
             ( (UINT64) 0 << 33) | /* first */
             ( (UINT64) 0 << 32) | /* last */
             ( (UINT64) icount << 16) | /* icount */
             ( (UINT64) *word_count );
      }
      Gen_Iolist_PutFieldConst (block, iolist_st, save_iotable_word1_offset,
				MTYPE_U8, word);
  }
}


/* ====================================================================
 *
 * WN *lower_io_statement (WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on fortran io statement.
 * Returned tree will always have a BLOCK node at the top.
 *
 * ==================================================================== */

WN *lower_io_statement (WN *tree, LOWER_ACTIONS actions)
{
  INT32 keytype;
  INT32 iolist;
  INT32 iomask;
  INT32 eeeflag = 0;
  INT32 iostat_flag = 0;
  INT32 eor_flag = 0;
  INT32 end_flag = 0;
  INT32 err_flag = 0;
  INT32 unit_flag;
  INT32 advance_flag = 0;
  INT32 is_direct = 0;
  INT32 is_internal_io;
  INT32 fmt_flag = 0;
  INT32 stk_size = 25;
  INT32	key_spec_item_size = 0;
  INT32	offset = 0;
  UINT64 word1;
  WN	**key_spec_items = NULL;
  INT32 nkeys = 0;
  INT64 matchtype;
  IOITEM ioitem_tmp;
  IOITEM unit_type;
  IOITEM format_type;
  BOOL errstat;
  BOOL endstat;
  FIOCLASS ioclass;
  SRCPOS srcpos;
  WN *wn;
  WN *wnx;
  WN *kid0;
  WN *unit_wn = NULL;
  WN *pure_unit_wn = NULL;
  WN *unit_len;
  WN *unit_rec;
  WN *kid1;
  WN *format_wn;
  WN *fmtsrc_wn = NULL;
  WN *parsfmt_wn = NULL;
  WN *size_wn = NULL;
  WN *advance_wn = NULL;
  WN *rec_wn = NULL;
  WN *varfmt = NULL;
  WN *varfmtfp;
  WN *block;
  WN *wn_tmp;
  WN *items[IOITEM_LAST + 1];
  WN *itemsx[IOITEM_LAST + 1];
  WN *iostat = NULL;
  WN *inqlength = NULL;
  WN *iostat1;
  WN *iostat2;
  WN *cilist_wn = NULL;
  WN *arg1;
  WN *arg2;
  WN *stack_wn = NULL;
  WN *dummy;
  ST *st, *stkey;
  LABEL_IDX err = 0;
  LABEL_IDX end = 0;
  LABEL_IDX eor = 0;
  ST *unit_ptr;
  IOSTATEMENT iostatement;
  TCON tcon;
  INT32 table_size;
  INT32 flflag;
  INT32 edflag = 0;
  TY_IDX ty;
  BOOL iostat_processing_not_done = TRUE;
  BOOL zero_escape_freq = FALSE;
  cilist_header_type *cilist_header_ptr;

#ifdef IO_DEBUG
USRCPOS s;
INT32 lineno;
s.srcpos = WN_Get_Linenum ( tree );
lineno = USRCPOS_linenum(s);
fprintf(stderr, "Processing I/O at line number %d\n", lineno);
#endif /* IO_DEBUG */
  /*  Perform some validation checking on arguments.  */

  Is_True(WN_opcode(tree) == OPC_IO,
	  ("expected io statement node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(Action(LOWER_IO_STATEMENT),
	  ("actions does not contain LOWER_IO_STATEMENT"));
  Is_True(WN_kid_count(tree) >= 2,
	  ("too few kids in io statement node"));
  Is_True(WN_io_item(WN_kid0(tree)) <= IOU_DOPE,
	  ("io statement kid0 not a unit IOITEM"));
  Is_True((WN_io_item(WN_kid1(tree)) >= IOF_NONE) &&
	  (WN_io_item(WN_kid1(tree)) <= IOF_CR_FMTSRC_DOPE),
	  ("io statement kid1 not a format IOITEM"));

  /*  Perform any necessary initialization for I/O statement.  */

  fio_dummy_count = 0;
  namelist_node = NULL;
  copyout_block = NULL;
  num_impl = 0;

  // Set up the current IO Library
  switch (WN_IO_Library(tree)) {
   case IOLIB_MIPS:
   case IOLIB_CRAY:
     current_io_library = WN_IO_Library(tree);
     break;
   default:
     Lmt_DevWarn(1,("I/O library unspecified"));
     current_io_library = (IOLIB) target_io_library;
     break;
  }

// >> WHIRL 0.30: Added MTYPE_A4 and MTYPE_A8 variants
// TODO WHIRL 0.30: get rid of MTYPE_U4 and MTYPE_U8 variants
  if (Pointer_Mtype == MTYPE_A4) {
    opc_lda   = OPC_A4LDA;
    opc_const = OPC_A4INTCONST;
    fcd_size = fiostructid_info[FID_CRAY_FCD].size32;
  } else if (Pointer_Mtype == MTYPE_A8) {
    opc_lda   = OPC_A8LDA;
    opc_const = OPC_A8INTCONST;
    fcd_size = fiostructid_info[FID_CRAY_FCD].size64;
  } else if (Pointer_Mtype == MTYPE_U4) {
    opc_lda   = OPC_U4LDA;
    opc_const = OPC_U4INTCONST;
    fcd_size = fiostructid_info[FID_CRAY_FCD].size32;
  } else {
    opc_lda   = OPC_U8LDA;
    opc_const = OPC_U8INTCONST;
    fcd_size = fiostructid_info[FID_CRAY_FCD].size64;
  }
// << WHIRL 0.30: Added MTYPE_A4 and MTYPE_A8 variants

  /*  Create BLOCK node which will contain all statements generated by
      lowering.  */

  block = WN_CreateBlock();

  if (Language == LANG_F77) {
    /*  Namelist I/O nodes do not generate runtime code but are retained and
        their information used when processing I/O statements referencing
        namelists.  */
    if (WN_io_statement(tree) == IOS_NAMELIST) {
      if (namelist_current_pu != Current_pu) {
            namelist_current_pu = Current_pu;
        namelist_node_list = NULL;
      }
      WN_next(tree) = namelist_node_list;
      namelist_node_list = tree;
      return (block);
    }
  }

  /*  Extract and process unit control item.  */

  kid0 = WN_kid0(tree);
  unit_type = (IOITEM) WN_io_item(kid0);
  switch (unit_type) {
    case IOU_NONE:
	unit_wn = NULL;
	unit_flag = 1;  /* An asterisk was specified */
	is_internal_io = 0;
	break;
    case IOU_DEFAULT:
	if (current_io_library == IOLIB_CRAY) {
#ifndef KEY // bug 8586
	  WN* unit = WN_kid0(kid0);
	  if (WN_operator_is(unit,OPR_INTCONST) ||
	               WN_operator_is(unit,OPR_LDID)) {
	     INT32 ret_type = WN_rtype(unit);
	     if (ret_type != MTYPE_I4 && ret_type != MTYPE_U4) {
	          Fail_FmtAssertion("unexpected unit type (%d) in I/O processing", ret_type);
             }
          }
#endif

          if (WN_operator_is(WN_kid0(kid0), OPR_INTCONST) && 
		WN_const_val(WN_kid0(kid0)) == 0) {
	    /* case WRITE(**, to write to stderr.  In this case, just
	    ** map the logical unit to unit 0 for now and don't set
	    ** the stdout flag */
	    unit_wn = create_pointer_to_node(block, WN_kid0(kid0),
					     (TY_IDX) 0, TRUE);
	    unit_flag = 0;
	  }
	  else {
	    unit_wn = NULL;
	    unit_flag = 1;  /* An asterisk was specified */
	  }
	  is_internal_io = 0;
	}
	else
	  unit_wn = extract_calls ( block, WN_kid0(kid0) );
	break;
    case IOU_EXTERNAL:
	if (current_io_library == IOLIB_MIPS)
	  unit_wn = extract_calls ( block, WN_kid0(kid0) );
        else {
        /* For BACKSPACE, REWIND, ENDFILE, OPEN, CLOSE and INQUIRE, F90 passes 
           the address of the unit item in kid0; in these cases we don't need 
           to pass the item thru create_pointer_to_node; we use pure_unit_wn
           to hold the unit_wn for later processing of units in these
           statements.
         */

	  pure_unit_wn = WN_kid0(kid0);
#ifndef KEY // bug 8586
	  if (WN_operator_is(pure_unit_wn,OPR_INTCONST) ||
	               WN_operator_is(pure_unit_wn,OPR_LDID)) {
	     INT32 ret_type = WN_rtype(pure_unit_wn);
	     if (ret_type != MTYPE_I4 && ret_type != MTYPE_U4) {
	          Fail_FmtAssertion("unexpected unit type (%d) in I/O processing", ret_type);
             }
          }
#endif

	  unit_wn = create_pointer_to_node(block, WN_kid0(kid0),
					   (TY_IDX) 0, TRUE);
	  unit_flag = 0;
          is_internal_io = 0;
        }
	break;
    case IOU_INTERNAL:
	if (current_io_library == IOLIB_MIPS) {
 	   unit_wn = extract_calls ( block, WN_kid0(kid0) );
	   if (WN_kid_count(kid0) >= 2)
	     unit_len = extract_calls ( block, WN_kid1(kid0) );
	   else
	     unit_len = NULL;
	   if (WN_kid_count(kid0) == 3)
	     unit_rec = extract_calls ( block, WN_kid2(kid0) );
	   else
	     unit_rec = NULL;
        } else {
           is_internal_io = 1;
	   /* 
	   ** Two cases here: one is CI_UNITCHAR for a simple character
	   ** variable and one is CI_UNITDOPEVEC for a character array
	   ** i.e. multiple record internal I/O
	   */
	   if (WN_kid_count( kid0 ) > 2) {
	     unit_flag = 3;
	     unit_wn = Create_Dope_From_IoItem( block, kid0 );
	   } else {
	     unit_flag = 2;
             unit_wn = Create_fcd(block, WN_kid0(kid0), WN_kid1(kid0));
	   }
        }
	break;
    case IOU_DOPE:
        is_internal_io = 1;
	unit_flag = 3;
	unit_wn = WN_kid0(kid0);

         /* The second kid of IOU_DOPE is just the address of the
            array from which the dope vector was created; this
            needs to be passed as a dummy arg on the call */

         dummy = WN_kid1(kid0);
         Add_To_Dummy_List(dummy);

	break;
    default:
	Fail_FmtAssertion("unexpected unit (%d) in I/O processing", unit_type);
  }
  WN_Delete ( kid0 );


  /*  Extract and process format control item.  */
  kid1 = WN_kid1(tree);
  format_type = (IOITEM) WN_io_item(kid1);
  switch (format_type) {
    case IOF_NONE:
	format_wn = NULL;
	fmtsrc_wn = NULL;
	fmt_flag = 0;
	break;
    case IOF_LIST_DIRECTED:
    case IOF_UNFORMATTED:
	format_wn = NULL;
	fmt_flag = 0;
	break;
    case IOF_CHAR_EXPR:
	format_wn = extract_calls ( block, WN_kid0(kid1) );
	if (current_io_library == IOLIB_CRAY) {
	  if (WN_kid_count(kid1) == 2) {
            fmtsrc_wn = Create_fcd(block, WN_kid0(kid1), WN_kid1(kid1) );
	    /* fmt_flag = 1; */
	  } else {
	    fmtsrc_wn = WN_kid0(kid1);
	    /* fmt_flag = 3; */
	  }
	  /* pretend that it is hollerith all the time so we won't need
	  ** to calculate the exact length for the character strings
	  ** representing the format as it could be any complicated
	  ** array/substring.  Hollerith will always work as the library
	  ** will match the first and the last parenthesis without
	  ** the constraint/limitation of the string length
	  */
	  fmt_flag = 3;
	}
	break;
    case IOF_LABEL:
    case IOF_ASSIGNED_VAR:
	format_wn = extract_calls ( block, WN_kid0(kid1) );
	if (current_io_library == IOLIB_CRAY) {
	  if (WN_kid_count(kid1) == 2) {
            fmtsrc_wn = Create_fcd(block, WN_kid0(kid1), WN_kid1(kid1) );
	    /* fmt_flag = 1; See comment above */
	  } else {
	    fmtsrc_wn = WN_kid0(kid1);
	    /* fmt_flag = 3; */
	  }
	  fmt_flag = 3;
	}
	break;
    case IOF_CR_FMTSRC:
        /* fmt_flag : TODO: this can have various settings */
        if (WN_kid_count(kid1) == 1 ) {
	  fmt_flag = 3; 
          fmtsrc_wn = WN_kid0(kid1);
        } else {
	   fmt_flag = 1;
           fmtsrc_wn = Create_fcd(block, WN_kid0(kid1), WN_kid1(kid1) );
        }
        break;
    case IOF_CR_FMTSRC_DOPE:
	 if (TY_is_character(Ty_Table [WN_ty(kid1)]))
	    fmt_flag = 2;
         else
	    fmt_flag = 4;
         fmtsrc_wn = WN_kid0(kid1);

         /* The second kid of IOF_CR_FMTSRC_DOPE is just the address of the
            array from which the dope vector was created; this
            needs to be passed as a dummy arg on the call */

         dummy = WN_kid1(kid1);
         Add_To_Dummy_List(dummy);

	 break;

    case IOF_NAMELIST_DIRECTED:
#ifndef KEY
        if (Language == LANG_F90) {
#else
        if (PU_f90_lang(Get_Current_PU())) {
#endif
	  INT32 i;
	  fmt_flag = 5;
	  for(i=0; i<WN_kid_count(kid1); i++)
	     Add_To_Dummy_List(WN_kid(kid1,i));
	} else {
	  format_wn = extract_calls ( block, WN_kid0(kid1) );
	  if (WN_operator(format_wn) != OPR_LDA)
	    Fail_FmtAssertion("namelist format error in I/O processing");
	  namelist_node = namelist_node_list;
	  while (namelist_node &&
	   ((WN_st(format_wn) != WN_st(WN_kid0(WN_kid1(namelist_node)))) ||
	    (WN_offset(format_wn) != WN_offset(WN_kid0(WN_kid1(namelist_node))))))
	    namelist_node = WN_next(namelist_node);
	  if (namelist_node == NULL)
	    Fail_FmtAssertion("cannot locate namelist in I/O processing");
	  fmt_flag = 5;
	}
	break;
    default:
	Fail_FmtAssertion("unexpected format (%d) in I/O processing",
			  format_type);
  }
  WN_Delete ( kid1 );


  /*  Clear all potential control items and then extract the ones specified.
      Do some partial pre-processing on specific items for efficiency.  */

  BZERO ( items,  sizeof(items)  );
  BZERO ( itemsx, sizeof(itemsx) );

  for (iolist=2; iolist<WN_kid_count(tree); iolist++) {

    wn_tmp = WN_kid(tree,iolist);
    ioitem_tmp = (IOITEM) WN_io_item(wn_tmp);
    if (ioitem_tmp >= IOL_ARRAY)
      break;

    switch (ioitem_tmp) {

      case IOC_END:
          end = WN_label_number(WN_kid0(wn_tmp));
	  if (current_io_library == IOLIB_CRAY)
             end_flag = 1;
	     // eeeflag |= 2;
	  break;

      case IOC_ERR:
          err = WN_label_number(WN_kid0(wn_tmp));
	  if (current_io_library == IOLIB_CRAY)
             err_flag = 1;
	     // eeeflag |= 1;
	  break;

      case IOC_EOR:
          eor = WN_label_number(WN_kid0(wn_tmp));
	  if (current_io_library == IOLIB_CRAY)
             eor_flag = 1;
	     // eeeflag |= 4;
	  break;

      case IOC_ERRFLAG:
	  items[ioitem_tmp] = WN_kid0(wn_tmp);
	  break;

      case IOC_READONLY:
      case IOC_SHARED:
	  items[ioitem_tmp] = extract_calls ( block, WN_kid0(wn_tmp) );
	  break;

      case IOC_KEY_START:
	  if (key_spec_item_size <= nkeys) {
	      if (key_spec_items == NULL)
		  key_spec_items = (WN **) malloc( (key_spec_item_size = 12) *
						  sizeof(WN *) );
	      else
		  key_spec_items = (WN **) realloc( key_spec_items,
						   (key_spec_item_size += 12) *
						   sizeof(WN *) );
	  }
	  key_spec_items[nkeys++] = extract_calls ( block, WN_kid0(wn_tmp) );
	  break;

      case IOC_KEY_CHARACTER:
      case IOC_KEY_INTEGER:
      case IOC_KEY_END:
	  key_spec_items[nkeys++] = extract_calls ( block, WN_kid0(wn_tmp) );
	  break;

      case IOC_REC:
	  if (current_io_library == IOLIB_MIPS) {
	     items[ioitem_tmp] = extract_calls ( block, WN_kid0(wn_tmp) );
	     if (WN_kid_count(wn_tmp) == 2)
	       itemsx[ioitem_tmp] = extract_calls ( block, WN_kid1(wn_tmp) );
          } else {
	     rec_wn = create_pointer_to_node(block, WN_kid0(wn_tmp),
					     (TY_IDX) 0, TRUE);
             is_direct = 1;
          }
	  break;
      case IOC_IOSTAT:
	  if (current_io_library == IOLIB_MIPS) 
	     iostat = extract_calls ( block, WN_kid0(wn_tmp) );
          else {
#ifdef KEY
	     // Under -IPA, different PUs may have different src_lang, but the 
	     // driver always passes -LANG:=ansi_c
	     if (PU_f90_lang(Get_Current_PU()))
#else
	     if (Language == LANG_F90)
#endif // KEY
               iostat = get_32bit_cilist_item(WN_kid0(wn_tmp), WN_ty(wn_tmp));
             else
	       iostat = WN_kid0(wn_tmp);
             iostat_flag = 1;
	     // eeeflag |= 8;
	     items[IOC_IOSTAT] = iostat;
	  }
	  break;
      case IOF_CR_PARSFMT:
	  /* Need to find out if create_pointer_to_node of this wn is needed */
          parsfmt_wn = create_pointer_to_node(block, WN_kid0(wn_tmp),
					      (TY_IDX) 0, TRUE);
          break;
      case IOC_SIZE:
#ifdef KEY
	  // Under -IPA, different PUs may have different src_lang, but the 
	  // driver always passes -LANG:=ansi_c
	  if (PU_f90_lang(Get_Current_PU())) {
#else
	  if (Language == LANG_F90) {
#endif // KEY
            size_wn = get_32bit_cilist_item(WN_kid0(wn_tmp), WN_ty(wn_tmp));
          } else {
	    size_wn = create_pointer_to_node(block, WN_kid0(wn_tmp),
					     (TY_IDX) 0, TRUE);
          }
	    
          break;
      case IOC_ADVANCE:
          kid0 = WN_kid0(wn_tmp);
          if (WN_operator(kid0) == OPR_LDA) {
	     char *str;
             st = WN_st(kid0);
	     if (ST_class(st) == CLASS_CONST) {
	       tcon = STC_val(st);
	       if (TCON_ty(tcon) == MTYPE_STRING) {
		   str = Index_to_char_array(TCON_cp(tcon));
		   if (*str=='y' || *str=='Y')
		      advance_flag = 0;
                   else if (*str=='n' || *str=='N')
		      advance_flag = 1;
               }
             } else {
	       advance_flag = 2;
             }
           } else {
	     advance_flag = 2;
           }
          advance_wn = Create_fcd(block, WN_kid0(wn_tmp), WN_kid1(wn_tmp));
	  break;

      case IOC_CR_EEEFLAG:
	  kid0 = WN_kid0(wn_tmp);
	  if (WN_operator(kid0) == OPR_INTCONST) {
	     eeeflag = WN_const_val(kid0);
             iostat_flag = ((eeeflag & 8) != 0) ? 1 : 0;
             eor_flag = ((eeeflag & 4) != 0) ? 1 : 0;
             end_flag = ((eeeflag & 2) != 0) ? 1 : 0;
             err_flag = ((eeeflag & 1) != 0) ? 1 : 0;
             
          } else {
	     Fail_FmtAssertion(
			"non constant value for control (%d) in I/O processing",
			ioitem_tmp);
          }
	  break;

      case IOC_CR_EDFLAG:
	  kid0 = WN_kid0(wn_tmp);
          if (WN_operator(kid0) == OPR_INTCONST) {
             edflag = WN_const_val(kid0);
             if (edflag == 0)
                break;
          }

          is_internal_io = 1;
          if (unit_flag == 0) {
            unit_wn = Create_fcd(block, WN_COPY_Tree(unit_wn), kid0);
            unit_flag = 2;
          } else if (unit_flag == 3) {
            if (Pointer_Size == 4) {
               wn = WN_CreateLdid(OPC_U4U4LDID, 0, WN_st(unit_wn),
                                            Be_Type_Tbl(MTYPE_U4));
            } else {
               wn = WN_CreateLdid(OPC_U8U8LDID, 0, WN_st(unit_wn),
                                            Be_Type_Tbl(MTYPE_U8));
            }
            unit_wn = Create_fcd(block, wn, kid0);
            unit_flag = 2;

          } else if (unit_flag == 2) {
             st = WN_st(WN_kid0(unit_wn));
             Gen_Io_PutFieldWN ( block, st, FCR_FCD_LEN, kid0);
          }
          edflag = 1;
          break;
 
      case IOC_CR_FLFLAG:
	  kid0 = WN_kid0(wn_tmp);
	  if (WN_operator(kid0) == OPR_INTCONST) {
	     flflag = WN_const_val(kid0);
          } else {
	     Fail_FmtAssertion(
			"non constant value for control (%d) in I/O processing",
			ioitem_tmp);
          }
	  break;

      case IOC_INQLENGTH_VAR:
          inqlength = WN_kid0(wn_tmp);
	  break;

          
      case IOC_VARFMT:
	  varfmt = WN_kid0(wn_tmp);
	  varfmtfp = WN_LdidPreg ( Pointer_type, Frame_Pointer_Preg_Offset );
	  break;

      case IOC_VARFMT_ORIGFMT:
	  WN_Delete ( WN_kid0(wn_tmp) );
	  break;

      case IOC_ASSOCIATEVARIABLE:
      case IOC_CARRIAGECONTROL:
      case IOC_DEFAULTFILE:
      case IOC_DISPOSE:
      case IOC_KEY:
      case IOC_KEYEQ:
      case IOC_KEYGE:
      case IOC_KEYGT:
      case IOC_KEYED:
      case IOC_KEYID:
      case IOC_MAXREC:
      case IOC_NML:
      case IOC_ORGANIZATION:
      case IOC_RECCOUNT:
      case IOC_RECORDTYPE:
      case IOC_TYPE:
      case IOC_U:
	  items[ioitem_tmp] = extract_calls ( block, WN_kid0(wn_tmp) );
	  if (WN_kid_count(wn_tmp) == 2)
	    itemsx[ioitem_tmp] = extract_calls ( block, WN_kid1(wn_tmp) );
	  break;

      case IOC_RECL:
      case IOC_EXIST:
      case IOC_OPENED:
      case IOC_NUMBER:
      case IOC_NAMED:
      case IOC_NEXTREC:
	  if (current_io_library == IOLIB_MIPS) {
	     items[ioitem_tmp] = extract_calls ( block, WN_kid0(wn_tmp) );
	     if (WN_kid_count(wn_tmp) == 2)
	        itemsx[ioitem_tmp] = extract_calls ( block, WN_kid1(wn_tmp) );
          } else {
#ifdef KEY
	     // Under -IPA, different PUs may have different src_lang, but the 
	     // driver always passes -LANG:=ansi_c
	     if (PU_f90_lang(Get_Current_PU())) {
#else
	     if (Language == LANG_F90) {
#endif // KEY
	       items[ioitem_tmp] = get_32bit_cilist_item(WN_kid0(wn_tmp), WN_ty(wn_tmp));
             } else {
	       items[ioitem_tmp] = create_pointer_to_node(block, WN_kid0(wn_tmp), (TY_IDX) 0, TRUE);
             }
		
          }
	  break;

      case IOC_FILE:
      case IOC_STATUS:
      case IOC_ACCESS:
      case IOC_FORM:
      case IOC_BLANK:
      case IOC_NAME:
      case IOC_SEQUENTIAL:
      case IOC_DIRECT:
      case IOC_FORMATTED:
      case IOC_UNFORMATTED:

	  if (current_io_library == IOLIB_MIPS) {
	     items[ioitem_tmp] = extract_calls ( block, WN_kid0(wn_tmp) );
	     if (WN_kid_count(wn_tmp) == 2)
	        itemsx[ioitem_tmp] = extract_calls ( block, WN_kid1(wn_tmp) );
          } else {
	     items[ioitem_tmp] = Create_fcd(block, WN_kid0(wn_tmp), WN_kid1(wn_tmp));
          }
	  break;

      case IOC_POSITION:
      case IOC_ACTION:
      case IOC_DELIM:
      case IOC_PAD:
      case IOC_READ:
      case IOC_WRITE:
      case IOC_READWRITE:
	   items[ioitem_tmp] = Create_fcd(block, WN_kid0(wn_tmp), WN_kid1(wn_tmp));
	   break;


      default:
	  Fail_FmtAssertion("unexpected control (%d) in I/O processing",
			    ioitem_tmp);

    }

    WN_Delete ( wn_tmp );

  }

  /*  Calculate error and end status.  */

  errstat = iostat != NULL || err != (LABEL_IDX) 0;
  endstat = iostat != NULL || err != (LABEL_IDX) 0 || end != (LABEL_IDX) 0;

  /*  Determine I/O statement classification based on unit, format and other
      control items. */

  if (unit_type == IOU_DEFAULT || unit_type == IOU_EXTERNAL)
    if (items[IOC_REC] == NULL)
      if (format_type == IOF_ASSIGNED_VAR || format_type == IOF_CHAR_EXPR ||
	  format_type == IOF_LABEL)
	ioclass = FCL_EXT_FORMATTED;
      else if (format_type == IOF_UNFORMATTED)
	ioclass = FCL_EXT_UNFORMATTED;
      else if (format_type == IOF_LIST_DIRECTED)
	ioclass = FCL_EXT_LIST;
      else if (format_type == IOF_NAMELIST_DIRECTED)
	ioclass = FCL_EXT_NAMELIST;
      else
	ioclass = FIOCLASS_NONE;
    else
      if (format_type == IOF_ASSIGNED_VAR || format_type == IOF_CHAR_EXPR ||
	  format_type == IOF_LABEL)
	ioclass = FCL_DIR_FORMATTED;
      else if (format_type == IOF_UNFORMATTED)
	ioclass = FCL_DIR_UNFORMATTED;
      else
	ioclass = FIOCLASS_NONE;
  else if (unit_type == IOU_INTERNAL)
    if (format_type == IOF_ASSIGNED_VAR || format_type == IOF_CHAR_EXPR ||
	format_type == IOF_LABEL)
      ioclass = FCL_INT_FORMATTED;
    else if (format_type == IOF_LIST_DIRECTED)
      ioclass = FCL_INT_LIST;
    else
      ioclass = FIOCLASS_NONE;
  else
    ioclass = FIOCLASS_NONE;

  /*  Retrieve feedback data  */
  if ( Cur_PU_Feedback && Cur_PU_Feedback->Same_in_out( tree ) )
    zero_escape_freq = TRUE;

  /*  Process the specific I/O statement.  */

  switch (iostatement = (IOSTATEMENT) WN_io_statement(tree)) {

    case IOS_BACKSPACE:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_ALIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSA_AERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSA_AUNIT, unit_wn );
	GEN_IO_CALL_1 ( block, FIO_BACKSPACE, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
	break;
      }
      else {
      /* For Cray I/O target then do IOS_CR_BACKSPACE */
	if (!items[IOC_ERRFLAG])
	  items[IOC_ERRFLAG] = WN_CreateIntconst ( OPC_I4INTCONST, 0);
      }

    case IOS_CR_BACKSPACE:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
	   unit_wn = pure_unit_wn;
        if (unit_wn != NULL) {
          arg1 = unit_wn;
        } else {
          if (Pointer_Size == 4)
            arg1 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
            arg1 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }

        if (iostat != NULL) {
           arg2 = iostat;
        } else {
          if (Pointer_Size == 4)
            arg2 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
            arg2 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }
           
        GEN_IO_CALL_3(block, FIO_CR_BACKSPACE, iostat1, iostat2, arg1, arg2, 
		       items[IOC_ERRFLAG]);
        break;

    case IOS_CLOSE:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_CLLIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSL_CLERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSL_CLUNIT, unit_wn );
	if (items[IOC_STATUS] != NULL)
	  Gen_Io_PutAddrWN ( block, st, FSL_CLSTA, items[IOC_STATUS] );
	else if (items[IOC_DISPOSE] != NULL)
	  Gen_Io_PutAddrWN ( block, st, FSL_CLSTA, items[IOC_DISPOSE] );
	else
	  Gen_Io_PutFieldConst ( block, st, FSL_CLSTA, (INT64) 0 );
	GEN_IO_CALL_1 ( block, FIO_CLOSE, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
	break;
      }

    case IOS_CR_CLOSE:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );
        st = Get_IoStruct_ST ( block, FID_CRAY_CLOSELIST, TRUE);

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
           unit_wn = pure_unit_wn;

	if (unit_wn != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_CLOSE_UNIT, unit_wn);
        if (iostat != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_CLOSE_IOSTAT, iostat);
        if (items[IOC_ERRFLAG] != NULL)
	   Gen_Io_PutFieldWN(block, st, FCR_CLOSE_ERR, items[IOC_ERRFLAG]);
        if (items[IOC_STATUS] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_CLOSE_STATUS, items[IOC_STATUS]);
        GEN_IO_CALL_1(block, FIO_CR_CLOSE, iostat1, iostat2, 
			Make_IoAddr_WN(st));
        break;

    case IOS_DELETE:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_ALIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSA_AERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSA_AUNIT, unit_wn );
	GEN_IO_CALL_1 ( block, FIO_DELETE, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
      } else {
    /* Calvin TODO */
      fprintf( stderr, "DELETE not yet implemented\n" );
      abort();
      }
	break;

    case IOS_ENDFILE:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_ALIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSA_AERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSA_AUNIT, unit_wn );
	GEN_IO_CALL_1 ( block, FIO_ENDFILE, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
	break;
      }
      else {
      /* Do IOS_CR_ENDFILE for Cray target */
	if (!items[IOC_ERRFLAG])
	  items[IOC_ERRFLAG] = WN_CreateIntconst ( OPC_I4INTCONST, 0);
      }

    case IOS_CR_ENDFILE:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
           unit_wn = pure_unit_wn;

        if (unit_wn != NULL) {
          arg1 = unit_wn;
        } else {
          if (Pointer_Size == 4)
            arg1 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
            arg1 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }

        if (iostat != NULL) {
           arg2 = iostat;
        } else {
          if (Pointer_Size == 4)
            arg2 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
            arg2 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }
           
        GEN_IO_CALL_3(block, FIO_CR_ENDFILE, iostat1, iostat2, arg1, arg2, 
		       items[IOC_ERRFLAG]);
        break;

    case IOS_FIND:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_FLIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSF_FERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSF_FUNIT, unit_wn );
	Gen_Io_PutFieldWN ( block, st, FSF_FREC, items[IOC_REC] );
	GEN_IO_CALL_1 ( block, FIO_FIND, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
      } else {
    /* Calvin TODO */
      fprintf( stderr, "FIND not yet implemented\n" );
      abort();
      }
	break;

    case IOS_INQUIRE:
      if (current_io_library == IOLIB_MIPS) {
	iomask = 0;
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_INLIST, TRUE );
	if (errstat)
	  Gen_Io_PutFieldConst ( block, st, FSN_INERR, 1 );
	if (unit_wn != NULL)
	  Gen_Io_PutFieldWN ( block, st, FSN_INUNIT, unit_wn );
	if (items[IOC_FILE] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INFILE, items[IOC_FILE] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INFILEN, itemsx[IOC_FILE] );
	}
	if (items[IOC_EXIST] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_EXIST, items[IOC_EXIST] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INEX, items[IOC_EXIST] );
        }
	if (items[IOC_OPENED] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_OPENED, items[IOC_OPENED] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INOPEN, items[IOC_OPENED] );
        }
	if (items[IOC_NUMBER] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_NUMBER, items[IOC_NUMBER] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INNUM, items[IOC_NUMBER] );
        }
	if (items[IOC_NAMED] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_NAMED, items[IOC_NAMED] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INNAMED, items[IOC_NAMED] );
        }
	if (items[IOC_NAME] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INNAME, items[IOC_NAME] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INNAMLEN, itemsx[IOC_NAME] );
	}
	if (items[IOC_ACCESS] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INACC, items[IOC_ACCESS] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INACCLEN, itemsx[IOC_ACCESS] );
	}
	if (items[IOC_SEQUENTIAL] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INSEQ, items[IOC_SEQUENTIAL] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INSEQLEN, itemsx[IOC_SEQUENTIAL] );
	}
	if (items[IOC_DIRECT] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INDIR, items[IOC_DIRECT] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INDIRLEN, itemsx[IOC_DIRECT] );
	}
	if (items[IOC_FORM] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INFMT, items[IOC_FORM] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INFMTLEN, itemsx[IOC_FORM] );
	}
	if (items[IOC_FORMATTED] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INFORM, items[IOC_FORMATTED] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INFORMLEN, itemsx[IOC_FORMATTED] );
	}
	if (items[IOC_UNFORMATTED] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INUNF, items[IOC_UNFORMATTED] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INUNFLEN,
			      itemsx[IOC_UNFORMATTED] );
	}
	if (items[IOC_RECL] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_RECL, items[IOC_RECL] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INRECL, items[IOC_RECL] );
        }
	if (items[IOC_NEXTREC] != NULL) {
	  Build_Io_Mask ( &iomask, FIM_NEXTREC, items[IOC_NEXTREC] );
	  Gen_Io_PutAddrWN ( block, st, FSN_INNREC, items[IOC_NEXTREC] );
        }
	if (items[IOC_BLANK] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INBLANK, items[IOC_BLANK] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INBLANKLEN, itemsx[IOC_BLANK] );
	}
	if (items[IOC_DEFAULTFILE] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INDEFAULTFILE,
			     items[IOC_DEFAULTFILE] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INDEFAULTFILELEN,
			      itemsx[IOC_DEFAULTFILE] );
	}
	if (items[IOC_CARRIAGECONTROL] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INCC, items[IOC_CARRIAGECONTROL] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INCCLEN,
			      itemsx[IOC_CARRIAGECONTROL] );
	}
	if (items[IOC_KEYED] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INKEYED, items[IOC_KEYED] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INKEYEDLEN, itemsx[IOC_KEYED] );
	}
	if (items[IOC_ORGANIZATION] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INORG, items[IOC_ORGANIZATION] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INORGLEN,
			      itemsx[IOC_ORGANIZATION] );
	}
	if (items[IOC_RECORDTYPE] != NULL) {
	  Gen_Io_PutAddrWN ( block, st, FSN_INRECORDTYPE,
			     items[IOC_RECORDTYPE] );
	  Gen_Io_PutFieldWN ( block, st, FSN_INRECORDTYPELEN,
			      itemsx[IOC_RECORDTYPE] );
	}
	GEN_IO_CALL_2 ( block, FIO_INQUIRE, iostat1, iostat2,
			Make_IoAddr_WN ( st ),
			WN_CreateIntconst ( OPC_I4INTCONST, iomask ) );
	break;
      }
      /* Do IOS_CR_INQUIRE for Cray target */

    case IOS_CR_INQUIRE:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );
        st = Get_IoStruct_ST ( block, FID_CRAY_INQLIST, TRUE);

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
           unit_wn = pure_unit_wn;

	if (unit_wn != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_INQ_UNIT, unit_wn);
        if (items[IOC_FILE] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_FILE, items[IOC_FILE]);
        if (iostat != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_INQ_IOSTAT, iostat);
        if (items[IOC_ERRFLAG] != NULL)
	   Gen_Io_PutFieldWN(block, st, FCR_INQ_ERR, items[IOC_ERRFLAG]);
        if (items[IOC_EXIST] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_EXIST, items[IOC_EXIST]);
        if (items[IOC_OPENED] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_OPENED, items[IOC_OPENED]);
        if (items[IOC_NUMBER] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_NUMBER, items[IOC_NUMBER]);
        if (items[IOC_NAMED] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_NAMED, items[IOC_NAMED]);
        if (items[IOC_NAME] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_NAME, items[IOC_NAME]);
        if (items[IOC_ACCESS] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_ACCESS, items[IOC_ACCESS]);
        if (items[IOC_SEQUENTIAL] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_SEQUENTIAL, items[IOC_SEQUENTIAL]);
        if (items[IOC_DIRECT] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_DIRECT, items[IOC_DIRECT]);
        if (items[IOC_FORM] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_FORM, items[IOC_FORM]);
        if (items[IOC_FORMATTED] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_FORMATTED, items[IOC_FORMATTED]);
        if (items[IOC_UNFORMATTED] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_UNFORMATTED, items[IOC_UNFORMATTED]);
        if (items[IOC_RECL] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_RECL, items[IOC_RECL]);
        if (items[IOC_NEXTREC] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_NEXTREC, items[IOC_NEXTREC]);
        if (items[IOC_BLANK] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_BLANK, items[IOC_BLANK]);
        if (items[IOC_POSITION] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_POSITION, items[IOC_POSITION]);
        if (items[IOC_ACTION] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_ACTION, items[IOC_ACTION]);
        if (items[IOC_READ] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_READ, items[IOC_READ]);
        if (items[IOC_WRITE] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_WRITE, items[IOC_WRITE]);
        if (items[IOC_READWRITE] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_READWRITE, items[IOC_READWRITE]);
        if (items[IOC_DELIM] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_DELIM, items[IOC_DELIM]);
        if (items[IOC_PAD] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_INQ_PAD, items[IOC_PAD]);
        GEN_IO_CALL_1(block, FIO_CR_INQUIRE, iostat1, iostat2, 
		       Make_IoAddr_WN(st));
        break;

    case IOS_DEFINEFILE:
    case IOS_OPEN:
	if (current_io_library == IOLIB_MIPS) {
	  iomask = 0;
	  process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			   err, end, (LABEL_IDX) 0, zero_escape_freq );
	  st = Get_IoStruct_ST ( block, FID_OLIST, TRUE );
	  if (errstat)
	    Gen_Io_PutFieldConst ( block, st, FSO_OERR, 1 );
	  Gen_Io_PutFieldWN ( block, st, FSO_OUNIT, unit_wn );
	  if (items[IOC_FILE] != NULL) {
	    Gen_Io_PutAddrWN ( block, st, FSO_OFNM, items[IOC_FILE] );
	    Gen_Io_PutFieldWN ( block, st, FSO_OFNMLEN, itemsx[IOC_FILE] );
	  } else if (items[IOC_NAME] != NULL) {
	    Gen_Io_PutAddrWN ( block, st, FSO_OFNM, items[IOC_NAME] );
	    Gen_Io_PutFieldWN ( block, st, FSO_OFNMLEN, itemsx[IOC_NAME] );
	  }
	  if (nkeys) {
	    Gen_Io_PutFieldWN ( block, st, FSO_ONKEYS, 
		  WN_CreateIntconst ( OPC_I4INTCONST, nkeys/3 ) );
	    stkey = Get_KeyStruct_ST ( nkeys );
	    Gen_Io_PutAddrWN ( block, st, FSO_OKEYS, Make_IoAddr_WN ( stkey ));
	    Gen_Io_PutKeyFieldWN ( block, stkey, key_spec_items, nkeys );
	    free ( key_spec_items );
	  }
	  if (items[IOC_TYPE] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OSTA, items[IOC_TYPE] );
	  if (items[IOC_STATUS] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OSTA, items[IOC_STATUS] );
	  if (items[IOC_ACCESS] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OACC, items[IOC_ACCESS] );
	  if (items[IOC_ORGANIZATION] != NULL) {
	    Gen_Io_PutAddrWN ( block, st, FSO_OORG, items[IOC_ORGANIZATION] );
	    Gen_Io_PutFieldWN ( block, st, FSN_INORGLEN,
			      itemsx[IOC_ORGANIZATION] );
	  }
	  if (items[IOC_FORM] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OFM, items[IOC_FORM] );
	  if (items[IOC_RECL])
	    Gen_Io_PutFieldWN ( block, st, FSO_ORL, items[IOC_RECL] );
	  if (items[IOC_BLANK] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OBLNK, items[IOC_BLANK] );
	  if (items[IOC_CARRIAGECONTROL] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_OCC, items[IOC_CARRIAGECONTROL] );
	  if (items[IOC_SHARED] != NULL)
	    Gen_Io_PutFieldConst ( block, st, FSO_OSHARED, 1 );
	  if (items[IOC_READONLY] != NULL)
	    Gen_Io_PutFieldConst ( block, st, FSO_OREADONLY, 1 );
	  if (items[IOC_ASSOCIATEVARIABLE] != NULL) {
	    Build_Io_Mask ( &iomask, FIM_ASSOCIATEVARIABLE,
			    items[IOC_ASSOCIATEVARIABLE] );
	    Gen_Io_PutAddrWN ( block, st, FSO_OASSOCV,
			       items[IOC_ASSOCIATEVARIABLE] );
          }
	  if (items[IOC_MAXREC])
	    Gen_Io_PutFieldWN ( block, st, FSO_OMAXREC, items[IOC_MAXREC] );
	  if (items[IOC_DEFAULTFILE] != NULL) {
	    Gen_Io_PutAddrWN ( block, st, FSO_ODFNM, items[IOC_DEFAULTFILE] );
	    Gen_Io_PutFieldWN ( block, st, FSO_ODFNMLEN,
			        itemsx[IOC_DEFAULTFILE] );
	  }
	  if (items[IOC_DISPOSE] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_ODISP, items[IOC_DISPOSE] );
	  if (items[IOC_RECORDTYPE] != NULL)
	    Gen_Io_PutAddrWN ( block, st, FSO_ORECTYPE, items[IOC_RECORDTYPE] );
	  GEN_IO_CALL_2 ( block, 
			  (iostatement == IOS_DEFINEFILE)
			    ? FIO_DEFINEFILE : FIO_OPEN, 
			  iostat1, iostat2,
			  Make_IoAddr_WN ( st ),
			  WN_CreateIntconst ( OPC_I4INTCONST, iomask ) );
	  break;
	}
	/* Do IOS_CR_OPEN for Cray target */

    case IOS_CR_OPEN:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );
        st = Get_IoStruct_ST ( block, FID_CRAY_OPENLIST, TRUE);

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
           unit_wn = pure_unit_wn;

	if (unit_wn != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_OPEN_UNIT, unit_wn);
        if (iostat != NULL)
	   Gen_Io_PutAddrWN (block, st, FCR_OPEN_IOSTAT, iostat);
        if (items[IOC_ERRFLAG] != NULL)
	   Gen_Io_PutFieldWN(block, st, FCR_OPEN_ERR, items[IOC_ERRFLAG]);
        if (items[IOC_FILE] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_FILE, items[IOC_FILE]);
        if (items[IOC_NAME] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_FILE, items[IOC_NAME]);
        if (items[IOC_STATUS] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_STATUS, items[IOC_STATUS]);
        if (items[IOC_ACCESS] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_ACCESS, items[IOC_ACCESS]);
        if (items[IOC_FORM] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_FORM, items[IOC_FORM]);
        if (items[IOC_RECL] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_RECL, items[IOC_RECL]);
        if (items[IOC_BLANK] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_BLANK, items[IOC_BLANK]);
        if (items[IOC_POSITION] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_POSITION, items[IOC_POSITION]);
        if (items[IOC_ACTION] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_ACTION, items[IOC_ACTION]);
        if (items[IOC_DELIM] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_DELIM, items[IOC_DELIM]);
        if (items[IOC_PAD] != NULL)
	   Gen_Io_PutAddrWN(block, st, FCR_OPEN_PAD, items[IOC_PAD]);
        GEN_IO_CALL_1(block, FIO_CR_OPEN, iostat1, iostat2, Make_IoAddr_WN(st));
        break;
	   
	
    case IOS_REWIND:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_ALIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSA_AERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSA_AUNIT, unit_wn );
	GEN_IO_CALL_1 ( block, FIO_REWIND, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
        break;
      } else {  /* IOLIB_CRAY */
	/* make sure that items[IOC_ERRFLAG] is not NULL and then
	   continue with the IOS_CR_REWIND processing*/
	items[IOC_ERRFLAG] = WN_CreateIntconst( OPC_I4INTCONST, 
				(err != (LABEL_IDX) 0) ? 1 : 0);
      }

    case IOS_CR_REWIND:
	process_iostat ( &iostat1, &iostat2, FALSE, iostat, err, end, eor,
			 zero_escape_freq );

#ifdef KEY
	// Under -IPA, different PUs may have different src_lang, but the driver
	// always passes -LANG:=ansi_c
	if (PU_f90_lang(Get_Current_PU()))
#else
	if (Language == LANG_F90)
#endif // KEY
           unit_wn = pure_unit_wn;

	if (unit_wn != NULL) {
	  arg1 = unit_wn;
        } else {
	  if (Pointer_Size == 4)
	    arg1 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
	    arg1 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }

	if (iostat != NULL) {
	   arg2 = iostat;
	} else {
	  if (Pointer_Size == 4)
	    arg2 = WN_CreateIntconst ( OPC_I4INTCONST, 0);
          else
	    arg2 = WN_CreateIntconst ( OPC_I8INTCONST, 0);
        }
	
	GEN_IO_CALL_3(block, FIO_CR_REWIND, iostat1, iostat2, arg1, arg2, 
		       items[IOC_ERRFLAG]);
        break;
	
    case IOS_UNLOCK:
      if (current_io_library == IOLIB_MIPS) {
	process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			 err, end, (LABEL_IDX) 0, zero_escape_freq );
	st = Get_IoStruct_ST ( block, FID_ALIST, FALSE );
	Gen_Io_PutFieldConst ( block, st, FSA_AERR, errstat );
	Gen_Io_PutFieldWN ( block, st, FSA_AUNIT, unit_wn );
	GEN_IO_CALL_1 ( block, FIO_UNLOCK, iostat1, iostat2,
			Make_IoAddr_WN ( st ) );
      }
      else {
    /* Calvin TODO */
      fprintf( stderr, "UNLOCK\n" );
      abort();
      }
	break;

    case IOS_INQLENGTH:
        if (PU_has_region(Get_Current_PU())) {
           cr_iostat1 = NULL;
           process_inqvar(&cr_iostat2, inqlength);
           iostat_processing_not_done = FALSE;
        }
	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
              cr_iostat1 = NULL;
              process_inqvar(&cr_iostat2, inqlength);
           }
           cilist_st = Gen_Temp_Symbol(MTYPE_To_TY(MTYPE_U8), "inquire_arg1");
           ty = ST_type(cilist_st);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 1;
	   } else {
             word1 = ((UINT64) 1 << 56) |  /* Version */
                     ((UINT64) stk_size << 16 ) |
                     ((UINT64) 1 ); // Number of words in control list struct
	   }
           WN_INSERT_BlockLast(block, 
                               WN_Stid (MTYPE_U8, 0, cilist_st, ty, 
                               WN_CreateIntconst (OPC_U8INTCONST,word1)));
        }

	ty = ST_type(cilist_st);
        cilist_wn = WN_CreateLda (opc_lda, 0, 
                                  Make_Pointer_Type(ty), 
                                  cilist_st); 
        lower_cray_io_items ( block, tree, iolist, WN_kid_count(tree),
                              TRUE, &table_size, flflag, cilist_wn, 
                              FIO_INQLENGTH);
        break;
    case IOS_CR_FRU:
	fmt_flag = 0;

        if (PU_has_region(Get_Current_PU())) {
           process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
           iostat_processing_not_done = FALSE;
        }

	// if first io call or in region and in middle of sequence of io calls
	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
	      process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			       err, end, eor, zero_escape_freq );
           }
	   cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
              cilist_header_ptr = (cilist_header_type *)&word1;
              cilist_header_ptr->version = 1;
              cilist_header_ptr->uflag = unit_flag;
              cilist_header_ptr->iostatflg = iostat_flag;
              cilist_header_ptr->eorflag = eor_flag;
              cilist_header_ptr->endflag = end_flag;
              cilist_header_ptr->errflag = err_flag;
              cilist_header_ptr->advcode = advance_flag;
              cilist_header_ptr->edcode = edflag;
              cilist_header_ptr->internal = is_internal_io;
              cilist_header_ptr->dflag = is_direct;
              cilist_header_ptr->fmt = fmt_flag;
              cilist_header_ptr->stksize = stk_size;
              cilist_header_ptr->icount = 7;
	   } else {
              word1 = ( (UINT64) 1 << 56 ) |  // Version
                      ( (UINT64) unit_flag << 48 ) |
                      ( (UINT64) eeeflag << 40 ) |
                      ( (UINT64) advance_flag << 35 ) |
                      ( (UINT64) edflag << 34 ) | // Encode decode flag
                      ( (UINT64) is_internal_io << 33 ) |
                      ( (UINT64) is_direct << 32 ) |
                      ( (UINT64) fmt_flag << 24 ) |
                      ( (UINT64) stk_size << 16 ) |
                      ( (UINT64) 7 ) ; // Number of words in control list struct
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
	   if (unit_wn != NULL)
  	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
	     Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
        cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
				   cilist_st);
        lower_cray_io_items ( block, tree, iolist, WN_kid_count(tree), TRUE, 
	                      &table_size, flflag, cilist_wn, 
			      FIO_CR_READ_UNFORMATTED);
	break;

    case IOS_CR_FRF:

        if (PU_has_region(Get_Current_PU())) {
           process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
	   iostat_processing_not_done = FALSE;
        }
	
	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
              process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			       err, end, eor, zero_escape_freq );
           }
           cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->uflag = unit_flag;
             cilist_header_ptr->iostatflg = iostat_flag;
             cilist_header_ptr->eorflag = eor_flag;
             cilist_header_ptr->endflag = end_flag;
             cilist_header_ptr->errflag = err_flag;
             cilist_header_ptr->advcode = advance_flag;
             cilist_header_ptr->edcode = edflag;
             cilist_header_ptr->internal = is_internal_io;
             cilist_header_ptr->dflag = is_direct;
             cilist_header_ptr->fmt = fmt_flag;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 7;
	   } else {
             word1 = ( (UINT64) 1 << 56 ) |  // Version
                     ( (UINT64) unit_flag << 48 ) |
                     ( (UINT64) eeeflag << 40 ) |
                     ( (UINT64) advance_flag << 35 ) |
                     ( (UINT64) edflag << 34 ) | // Encode decode flag
                     ( (UINT64) is_internal_io << 33 ) |
                     ( (UINT64) is_direct << 32 ) |
                     ( (UINT64) fmt_flag << 24 ) |
                     ( (UINT64) stk_size << 16 ) |
                     ( (UINT64) 7 ) ; // Number of words in control list struct
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
           if (unit_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
	cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                   cilist_st);
        lower_cray_io_items ( block, tree, iolist, WN_kid_count(tree),
                              TRUE, &table_size, flflag, cilist_wn, 
                              FIO_CR_READ_FORMATTED);
	break;
	
    case IOS_CR_FWU:
	fmt_flag = 0;

        if (PU_has_region(Get_Current_PU())) {
           process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
           iostat_processing_not_done = FALSE;
        }

	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
	     process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			      err, end, eor, zero_escape_freq );
           }
           cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->uflag = unit_flag;
             cilist_header_ptr->iostatflg = iostat_flag;
             cilist_header_ptr->eorflag = eor_flag;
             cilist_header_ptr->endflag = end_flag;
             cilist_header_ptr->errflag = err_flag;
             cilist_header_ptr->advcode = advance_flag;
             cilist_header_ptr->edcode = edflag;
             cilist_header_ptr->internal = is_internal_io;
             cilist_header_ptr->dflag = is_direct;
             cilist_header_ptr->fmt = fmt_flag;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 7;
	   } else {
             word1 = ( (UINT64) 1 << 56 ) |  // Version
                     ( (UINT64) unit_flag << 48 ) |
                     ( (UINT64) eeeflag << 40 ) |
                     ( (UINT64) advance_flag << 35 ) |
                     ( (UINT64) edflag << 34 ) | // Encode decode flag
                     ( (UINT64) is_internal_io << 33 ) |
                     ( (UINT64) is_direct << 32 ) |
                     ( (UINT64) fmt_flag << 24 ) |
                     ( (UINT64) stk_size << 16 ) |
                     ( (UINT64) 7 ) ; // Number of words in control list struct
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
           if (unit_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
	cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
        lower_cray_io_items ( block, tree, iolist, WN_kid_count(tree),
                              TRUE, &table_size, flflag, cilist_wn, 
                              FIO_CR_WRITE_UNFORMATTED);
	break;

    case IOS_CR_FWF:

        if (PU_has_region(Get_Current_PU())) {
	   process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
           iostat_processing_not_done = FALSE;
        }

	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
              process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			       err, end, eor, zero_escape_freq );
           }
           cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->uflag = unit_flag;
             cilist_header_ptr->iostatflg = iostat_flag;
             cilist_header_ptr->eorflag = eor_flag;
             cilist_header_ptr->endflag = end_flag;
             cilist_header_ptr->errflag = err_flag;
             cilist_header_ptr->advcode = advance_flag;
             cilist_header_ptr->edcode = edflag;
             cilist_header_ptr->internal = is_internal_io;
             cilist_header_ptr->dflag = is_direct;
             cilist_header_ptr->fmt = fmt_flag;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 7;
	   } else {
             word1 = ( (UINT64) 1 << 56 ) |  // Version
                     ( (UINT64) unit_flag << 48 ) |
                     ( (UINT64) eeeflag << 40 ) |
                     ( (UINT64) advance_flag << 35 ) |
                     ( (UINT64) edflag << 34 ) | // Encode decode flag
                     ( (UINT64) is_internal_io << 33 ) |
                     ( (UINT64) is_direct << 32 ) |
                     ( (UINT64) fmt_flag << 24 ) |
                     ( (UINT64) stk_size << 16 ) |
                     ( (UINT64) 7 ) ; // Number of words in control list struct
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
           if (unit_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
	cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
        lower_cray_io_items ( block, tree, iolist, WN_kid_count(tree),
                              TRUE, &table_size, flflag, cilist_wn, 
                              FIO_CR_WRITE_FORMATTED);
	break;

    case IOS_CR_FRN:

	fmt_flag = 5;

        if (PU_has_region(Get_Current_PU())) {
           process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
           iostat_processing_not_done = FALSE;
        }

	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
	      process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			       err, end, eor, zero_escape_freq );
           }
           cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->uflag = unit_flag;
             cilist_header_ptr->iostatflg = iostat_flag;
             cilist_header_ptr->eorflag = eor_flag;
             cilist_header_ptr->endflag = end_flag;
             cilist_header_ptr->errflag = err_flag;
             cilist_header_ptr->advcode = advance_flag;
             cilist_header_ptr->edcode = edflag;
             cilist_header_ptr->internal = is_internal_io;
             cilist_header_ptr->dflag = is_direct;
             cilist_header_ptr->fmt = fmt_flag;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 7;
	   } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) advance_flag << 35 ) |
                ( (UINT64) edflag << 34 ) | /* Encode decode flag */
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
           if (unit_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
	cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
        if (stack_ty == (TY_IDX) 0)
           stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));

        if (Current_pu != cray_iolist_current_pu) {
           cray_iolist_current_pu = Current_pu;
           stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
	   container_block_for_iolists = NULL;
	   num_iolists = 0;
        }

        stack_wn = WN_CreateLda (opc_lda, 0,
                           Make_Pointer_Type (ST_type(stack_st), FALSE),
                           stack_st);
        if (PU_has_region(Get_Current_PU())) {
          /* We are in a region; Stuff block2 after each call instead of
             stuffing block1 after all (but last) call and generating a
             branch from it to block2; the reason we do it this way is
             because generation of block1 requires the IO lowerer to
             generate a new label, which is not exposed to region processing.
           */
          GEN_IO_CALL_3 ( block, FIO_CR_READ_NAMELIST, NULL, cr_iostat2, cilist_wn,
                          WN_kid0(WN_kid(tree,iolist)), stack_wn);
        } else {
            if (LAST_CALL(flflag)) {
    	       GEN_IO_CALL_3 ( block, FIO_CR_READ_NAMELIST, NULL, 
    			       cr_iostat2, cilist_wn, 
    			       WN_kid0(WN_kid(tree,iolist)), stack_wn);
            } else {
               GEN_IO_CALL_3 ( block, FIO_CR_READ_NAMELIST, cr_iostat1, 
    			       NULL, cilist_wn, 
    			       WN_kid0(WN_kid(tree,iolist)), stack_wn);
            }
        }
	break;

    case IOS_CR_FWN:
	fmt_flag = 5;

        if (PU_has_region(Get_Current_PU())) {
           process_iostat ( &cr_iostat1, &cr_iostat2, FALSE, iostat,
			    err, end, eor, zero_escape_freq );
           iostat_processing_not_done = FALSE;
        }

	if (FIRST_CALL(flflag) || (cilist_st == NULL)) {
           if (iostat_processing_not_done) {
              process_iostat ( &cr_iostat1, &cr_iostat2, TRUE, iostat,
			       err, end, eor, zero_escape_freq );
           }
           cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
           word1 = 0;
	   if (Target_Byte_Sex == LITTLE_ENDIAN) {
             cilist_header_ptr = (cilist_header_type *)&word1;
             cilist_header_ptr->version = 1;
             cilist_header_ptr->uflag = unit_flag;
             cilist_header_ptr->iostatflg = iostat_flag;
             cilist_header_ptr->eorflag = eor_flag;
             cilist_header_ptr->endflag = end_flag;
             cilist_header_ptr->errflag = err_flag;
             cilist_header_ptr->advcode = advance_flag;
             cilist_header_ptr->edcode = edflag;
             cilist_header_ptr->internal = is_internal_io;
             cilist_header_ptr->dflag = is_direct;
             cilist_header_ptr->fmt = fmt_flag;
             cilist_header_ptr->stksize = stk_size;
             cilist_header_ptr->icount = 7;
	   } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) advance_flag << 35 ) |
                ( (UINT64) edflag << 34 ) | /* Encode decode flag */
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	   }
           Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
           if (unit_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_UNIT, unit_wn );
           if (iostat != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_IOSTAT, iostat);
           if (rec_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_REC, rec_wn );
           if (parsfmt_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_PARSFMT, parsfmt_wn );
           if (fmtsrc_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_FMTSRC, fmtsrc_wn );
           if (advance_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_ADVANCE, advance_wn );
           if (size_wn != NULL)
             Gen_Io_PutAddrWN ( block, cilist_st, FCR_CI_SIZE, size_wn );
        }
	cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
        if (stack_ty == (TY_IDX) 0)
           stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));

        if (Current_pu != cray_iolist_current_pu) {
           cray_iolist_current_pu = Current_pu;
           stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
	   container_block_for_iolists = NULL;
	   num_iolists = 0;
        }

        stack_wn = WN_CreateLda (opc_lda, 0,
                           Make_Pointer_Type (ST_type(stack_st), FALSE),
                           stack_st);
        if (PU_has_region(Get_Current_PU())) {
          /* We are in a region; Stuff block2 after each call instead of
             stuffing block1 after all (but last) call and generating a
             branch from it to block2; the reason we do it this way is
             because generation of block1 requires the IO lowerer to
             generate a new label, which is not exposed to region processing.
           */
          GEN_IO_CALL_3 ( block, FIO_CR_WRITE_NAMELIST, NULL, cr_iostat2, cilist_wn,
                          WN_kid0(WN_kid(tree,iolist)), stack_wn);
        } else {
           if (LAST_CALL(flflag)) {
   	      GEN_IO_CALL_3 ( block, FIO_CR_WRITE_NAMELIST, NULL, 
   			      cr_iostat2, cilist_wn, 
   			      WN_kid0(WN_kid(tree,iolist)), stack_wn);
           } else {
              GEN_IO_CALL_3 ( block, FIO_CR_WRITE_NAMELIST, cr_iostat1, 
   			      NULL, cilist_wn, 
   			      WN_kid0(WN_kid(tree,iolist)), stack_wn);
           }
        }
	break;

    case IOS_ACCEPT:
    case IOS_DECODE:
    case IOS_READ:

	switch (ioclass) {

	  case FCL_EXT_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutAddrWN ( block, st, FSC_CIFMT, format_wn );
	      if (items[IOC_KEY] != NULL) {
		keytype = IOC_KEY;
		matchtype = ISEQUAL;
	      } else if (items[IOC_KEYEQ] != NULL) {
		keytype = IOC_KEYEQ;
		matchtype = ISEQUAL;
	      } else if (items[IOC_KEYGE] != NULL) {
		keytype = IOC_KEYGE;
		matchtype = ISGTEQ;
	      } else if (items[IOC_KEYGT] != NULL) {
		keytype = IOC_KEYGT;
		matchtype = ISGREAT;
	      } else
		keytype = 0;
	      if (keytype) {
	        if (itemsx[keytype]) {
		  /* Character key */
		  Gen_Io_PutAddrWN ( block, st, FSC_CIKEYVAL, items[keytype] );
		  Gen_Io_PutFieldWN ( block, st, FSC_CIKEYVALLEN,
				      itemsx[keytype] );
		  Gen_Io_PutFieldConst( block, st, FSC_CIKEYTYPE, CHARTYPE );
	        } else {
		  /*
		  The general algorithm doesn't work in this case as
		  the type is forced to be INTEGER*4 in all cases
		  Gen_Io_PutFieldWN ( block, st, FSC_CIKEYVAL, items[keytype] );
		  */
		  INT32 foffset = (Pointer_Size == 4)
				? fiostruct_info[FSC_CIKEYVAL].offset32
				: fiostruct_info[FSC_CIKEYVAL].offset64;
		  wnx = WN_CreateStid ( OPC_I4STID, foffset, st, 
				Be_Type_Tbl(MTYPE_I4), items[keytype] );
		  WN_INSERT_BlockLast ( block, wnx );
		  Gen_Io_PutFieldConst( block, st, FSC_CIKEYTYPE, LONGTYPE );
		}
		Gen_Io_PutFieldConst( block, st, FSC_CIMATCH, matchtype );
	      }
	      else {
		/* clear the field to zero for non-indexed I/O */
		Gen_Io_PutFieldConst( block, st, FSC_CIMATCH, 0 );
	        Gen_Io_PutFieldConst ( block, st, FSC_CIKEYTYPE, 0 );
	      }
	      if (items[IOC_KEYID] != NULL)
		Gen_Io_PutFieldWN ( block, st, FSC_CIKEYID, items[IOC_KEYID] );
	      else
		Gen_Io_PutFieldConst( block, st, FSC_CIKEYID, -1L );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_FORMAT_end, iostat1,
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_READ_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//	      eeeflag = (errstat != FALSE)
//			| ((end != (LABEL_IDX) 0) << 1)
//			| ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_EXT_UNFORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      if (items[IOC_KEY] != NULL) {
		keytype = IOC_KEY;
		matchtype = ISEQUAL;
	      } else if (items[IOC_KEYEQ] != NULL) {
		keytype = IOC_KEYEQ;
		matchtype = ISEQUAL;
	      } else if (items[IOC_KEYGE] != NULL) {
		keytype = IOC_KEYGE;
		matchtype = ISGTEQ;
	      } else if (items[IOC_KEYGT] != NULL) {
		keytype = IOC_KEYGT;
		matchtype = ISGREAT;
	      } else
		keytype = 0;
	      if (keytype) {
	        if (itemsx[keytype]) {
		  Gen_Io_PutAddrWN ( block, st, FSC_CIKEYVAL, items[keytype] );
		  Gen_Io_PutFieldWN ( block, st, FSC_CIKEYVALLEN,
				      itemsx[keytype] );
		  Gen_Io_PutFieldConst( block, st, FSC_CIKEYTYPE, CHARTYPE );
	        } else {
		  /*
		  The general algorithm doesn't work in this case as
		  the type is forced to be INTEGER*4 in all cases
		  Gen_Io_PutFieldWN ( block, st, FSC_CIKEYVAL, items[keytype] );
		  */
		  INT32 foffset = (Pointer_Size == 4)
				? fiostruct_info[FSC_CIKEYVAL].offset32
				: fiostruct_info[FSC_CIKEYVAL].offset64;
		  wnx = WN_CreateStid ( OPC_I4STID, foffset, st, 
				Be_Type_Tbl(MTYPE_I4), items[keytype] );
		  WN_INSERT_BlockLast ( block, wnx );
		  Gen_Io_PutFieldConst( block, st, FSC_CIKEYTYPE, LONGTYPE );
		}
		Gen_Io_PutFieldConst( block, st, FSC_CIMATCH, matchtype );
	      }
	      else {
		Gen_Io_PutFieldConst( block, st, FSC_CIMATCH, 0 );
	        Gen_Io_PutFieldConst ( block, st, FSC_CIKEYTYPE, 0 );
	      }
	      if (items[IOC_KEYID] != NULL)
		Gen_Io_PutFieldWN ( block, st, FSC_CIKEYID, items[IOC_KEYID] );
	      else
		Gen_Io_PutFieldConst( block, st, FSC_CIKEYID, -1L );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_READ_UNFORMAT_start, 
			      iostat1, NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_UNFORMAT_start, 
			      iostat1, NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_UNFORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_UNFORMAT_end, iostat1,
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_READ_UNFORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_UNFORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_EXT_LIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIKEYTYPE, 0 );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
	        GEN_IO_CALL_2 ( block, FIO_EXT_READ_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ),
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_LIST, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_LIST_end, iostat1, 
				iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_READ_LIST_end, iostat1, 
				iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_EXT_NAMELIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutAddrWN ( block, st, FSC_CINML, format_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
	        GEN_IO_CALL_2 ( block, FIO_EXT_READ_NAMELIST_start, iostat1,
			      iostat2, Make_IoAddr_WN ( st ),
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_READ_NAMELIST_start, iostat1,
			      iostat2, Make_IoAddr_WN ( st ) );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
              cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = iostat_flag;
                cilist_header_ptr->eorflag = eor_flag;
                cilist_header_ptr->endflag = end_flag;
                cilist_header_ptr->errflag = err_flag;
                cilist_header_ptr->advcode = advance_flag;
                cilist_header_ptr->edcode = edflag;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) advance_flag << 35 ) |
                ( (UINT64) edflag << 34 ) | /* Encode decode flag */
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
              Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, cilist_st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
	      cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
              if (stack_ty == (TY_IDX) 0)
                stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));

              if (Current_pu != cray_iolist_current_pu) {
                cray_iolist_current_pu = Current_pu;
                stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
		io_set_addr_passed_flag(stack_st);
              }

              stack_wn = WN_CreateLda (opc_lda, 0,
                           Make_Pointer_Type (ST_type(stack_st), FALSE),
                           stack_st);

	      GEN_IO_CALL_3 ( block, FIO_CR_READ_NAMELIST, NULL, 
			    iostat2, cilist_wn, 
			    format_wn, stack_wn);
	    }
	    break;

	  case FCL_INT_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_ICILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIERR, errstat );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIEND, endstat );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIFMT, format_wn );
	      if (unit_len != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRLEN, unit_len );
	      else if (WN_opcode(unit_wn) == OPC_U4LDA ||
		       WN_opcode(unit_wn) == OPC_U8LDA)
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRLEN,
                  TY_size (Ty_Table [ST_type (WN_st(unit_wn))])
                                     );
	      else
		Fail_FmtAssertion(
			"unexpected decode length (%s) in I/O processing",
			OPCODE_name(WN_opcode(unit_wn)));
	      if (unit_rec != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRNUM, unit_rec );
	      else
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRNUM, 1 );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSI_ICIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSI_ICIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
	        GEN_IO_CALL_2 ( block, FIO_INT_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ),
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_INT_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
	        GEN_IO_CALL_1 ( block, FIO_INT_READ_FORMAT_end, iostat1,
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_INT_READ_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//	      eeeflag = (errstat != FALSE)
//			| ((end != (LABEL_IDX) 0) << 1)
//			| ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->edcode = edflag;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) edflag << 34 ) | /* Encode decode flag */
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Gen_Io_PutAddrWN ( block, st, FCR_CI_FMTSRC, format_wn );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_INT_LIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_ICILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIERR, errstat );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIEND, endstat );
	      if (unit_len != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRLEN, unit_len );
	      else if (WN_opcode(unit_wn) == OPC_U4LDA ||
		       WN_opcode(unit_wn) == OPC_U8LDA)
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRLEN,
                  TY_size (Ty_Table [ST_type (WN_st(unit_wn))])
                                     );
	      else
		Fail_FmtAssertion(
			"unexpected decode length (%s) in I/O processing",
			OPCODE_name(WN_opcode(unit_wn)));
	      if (unit_rec != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRNUM, unit_rec );
	      else
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRNUM, 1 );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
	        GEN_IO_CALL_2 ( block, FIO_INT_READ_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ),
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_INT_READ_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_LIST, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
	        GEN_IO_CALL_1 ( block, FIO_INT_READ_LIST_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_INT_READ_LIST_end, iostat1, 
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_DIR_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutAddrWN ( block, st, FSC_CIFMT, format_wn );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIREC, items[IOC_REC] );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_DIR_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_DIR_READ_FORMAT_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_DIR_READ_FORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_DIR_READ_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_DIR_UNFORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIREC, items[IOC_REC] );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_DIR_READ_UNFORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_DIR_READ_UNFORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_UNFORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_DIR_READ_UNFORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_DIR_READ_UNFORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_READ_UNFORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  default:
	      Fail_FmtAssertion(
			"unexpected I/O statement (%d) in I/O processing",
			ioclass);

	}
	
	break;

    case IOS_ENCODE:
    case IOS_PRINT:
    case IOS_TYPE:
    case IOS_WRITE:

	switch (ioclass) {

	  case FCL_EXT_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutAddrWN ( block, st, FSC_CIFMT, format_wn );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_FORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_WRITE_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_EXT_UNFORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_WRITE_UNFORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_UNFORMAT_start, iostat1,
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_UNFORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_UNFORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_WRITE_UNFORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_UNFORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_EXT_LIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_WRITE_LIST_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_LIST, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_LIST_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_WRITE_LIST_end, iostat1, 
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_EXT_NAMELIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIEND, endstat );
	      Gen_Io_PutAddrWN ( block, st, FSC_CINML, format_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_WRITE_NAMELIST_start, iostat1, 
			      iostat2, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_WRITE_NAMELIST_start, iostat1,
			      iostat2, Make_IoAddr_WN ( st ) );
	    } else {
	      process_iostat ( &iostat1, &iostat2, FALSE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
              cilist_st = Get_IoStruct_ST ( block, FID_CRAY_CLIST, TRUE);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = iostat_flag;
                cilist_header_ptr->eorflag = eor_flag;
                cilist_header_ptr->endflag = end_flag;
                cilist_header_ptr->errflag = err_flag;
                cilist_header_ptr->advcode = advance_flag;
                cilist_header_ptr->edcode = edflag;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) advance_flag << 35 ) |
                ( (UINT64) edflag << 34 ) | /* Encode decode flag */
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
              Gen_Io_PutFieldConst ( block, cilist_st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, cilist_st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );
	      cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(cilist_st)), 
                                  cilist_st);
              if (stack_ty == (TY_IDX) 0)
                stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));

              if (Current_pu != cray_iolist_current_pu) {
                cray_iolist_current_pu = Current_pu;
                stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
		io_set_addr_passed_flag(stack_st);
              }

              stack_wn = WN_CreateLda (opc_lda, 0,
                           Make_Pointer_Type (ST_type(stack_st), FALSE),
                           stack_st);

	      GEN_IO_CALL_3 ( block, FIO_CR_WRITE_NAMELIST, NULL, 
			    iostat2, cilist_wn, 
			    format_wn, stack_wn);
	    }
	    break;

	  case FCL_INT_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_ICILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIERR, errstat );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIUNIT, unit_wn );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIFMT, format_wn );
	      if (unit_len != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRLEN, unit_len );
	      else if (WN_opcode(unit_wn) == OPC_U4LDA ||
		       WN_opcode(unit_wn) == OPC_U8LDA)
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRLEN,
                  TY_size (Ty_Table [ST_type (WN_st(unit_wn))])
                                     );
	      else
		Fail_FmtAssertion(
			"unexpected decode length (%s) in I/O processing",
			OPCODE_name(WN_opcode(unit_wn)));
	      if (unit_rec != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRNUM, unit_rec );
	      else
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRNUM, 1 );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSI_ICIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSI_ICIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_INT_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_INT_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_INT_WRITE_FORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_INT_WRITE_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_INT_LIST:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_ICILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSI_ICIERR, errstat );
	      Gen_Io_PutAddrWN ( block, st, FSI_ICIUNIT, unit_wn );
	      if (unit_len != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRLEN, unit_len );
	      else if (WN_opcode(unit_wn) == OPC_U4LDA ||
		       WN_opcode(unit_wn) == OPC_U8LDA)
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRLEN,
		       TY_size (Ty_Table [ST_type (WN_st(unit_wn))])
				     );

	      else
		Fail_FmtAssertion(
			"unexpected decode length (%s) in I/O processing",
			OPCODE_name(WN_opcode(unit_wn)));
	      if (unit_rec != NULL)
		Gen_Io_PutFieldWN ( block, st, FSI_ICIRNUM, unit_rec );
	      else
		Gen_Io_PutFieldConst ( block, st, FSI_ICIRNUM, 1 );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_INT_WRITE_LIST_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_INT_WRITE_LIST_start, iostat1, NULL,
			      Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_LIST, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_INT_WRITE_LIST_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_INT_WRITE_LIST_end, iostat1, 
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	    break;

	  case FCL_DIR_FORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutAddrWN ( block, st, FSC_CIFMT, format_wn );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIREC, items[IOC_REC] );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_DIR_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_DIR_WRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_DIR_WRITE_FORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_DIR_WRITE_FORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_FORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  case FCL_DIR_UNFORMATTED:
	    if (current_io_library == IOLIB_MIPS) {
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIREC, items[IOC_REC] );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_DIR_WRITE_UNFORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_DIR_WRITE_UNFORMAT_start, iostat1,
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_UNFORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_DIR_WRITE_UNFORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_DIR_WRITE_UNFORMAT_end, iostat1,
			      iostat2 );
	    } else { /* IOLIB_CRAY */
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST( block, FID_CRAY_CLIST, TRUE );
//            eeeflag = (errstat != FALSE)
//                      | ((end != (LABEL_IDX) 0) << 1)
//                      | ((iostat != NULL) << 3);
              word1 = 0;
	      if (Target_Byte_Sex == LITTLE_ENDIAN) {
                cilist_header_ptr = (cilist_header_type *)&word1;
                cilist_header_ptr->version = 1;
                cilist_header_ptr->uflag = unit_flag;
                cilist_header_ptr->iostatflg = (iostat != NULL) ? 1 : 0;
                cilist_header_ptr->endflag = (end != (LABEL_IDX) 0) ? 1 : 0;
                cilist_header_ptr->errflag = (errstat != FALSE) ? 1 : 0;
                cilist_header_ptr->internal = is_internal_io;
                cilist_header_ptr->dflag = is_direct;
                cilist_header_ptr->fmt = fmt_flag;
                cilist_header_ptr->stksize = stk_size;
                cilist_header_ptr->icount = 7;
	      } else {
           word1 = ( (UINT64) 1 << 56 ) |  /* Version */
                ( (UINT64) unit_flag << 48 ) |
                ( (UINT64) eeeflag << 40 ) |
                ( (UINT64) is_internal_io << 33 ) |
                ( (UINT64) is_direct << 32 ) |
                ( (UINT64) fmt_flag << 24 ) |
                ( (UINT64) stk_size << 16 ) |
                ( (UINT64) 7 ) ; /* Number of words in control list struct */
	      }
	      Gen_Io_PutFieldConst( block, st, FCR_CI_WORD1, word1 );
	      Set_Cilist_Fields( block, st, unit_wn, items, rec_wn, 
				parsfmt_wn, fmtsrc_wn, advance_wn, 
				size_wn, varfmt );

              cilist_wn = WN_CreateLda (opc_lda, 0, TY_pointer(ST_type(st)), st);
              lower_f77_io_items ( block, tree, cilist_wn, iostat1, iostat2,
				FIO_CR_WRITE_UNFORMATTED, FALSE, &offset,
				iolist, WN_kid_count(tree));
	    }
	      break;

	  default:
	      Fail_FmtAssertion(
			"unexpected I/O statement (%d) in I/O processing",
			ioclass);

	}
	
	break;

    case IOS_REWRITE:

      if (current_io_library == IOLIB_MIPS) {
	switch (ioclass) {

	  case FCL_EXT_FORMATTED:
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, FALSE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      Gen_Io_PutAddrWN ( block, st, FSC_CIFMT, format_wn );
	      if (varfmt != NULL) {
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMT, varfmt );
	        Gen_Io_PutAddrWN ( block, st, FSC_CIVFMTFP, varfmtfp );
	      }
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_REWRITE_FORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_FORMAT_start, iostat1,
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_FORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_FORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_REWRITE_FORMAT_end, iostat1,
			      iostat2 );
	      break;

	  case FCL_EXT_UNFORMATTED:
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, TRUE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_REWRITE_UNFORMAT_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_UNFORMAT_start, iostat1,
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_UNFORMAT, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_UNFORMAT_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_REWRITE_UNFORMAT_end, iostat1,
			      iostat2 );
	      break;

	  case FCL_EXT_LIST:
	      process_iostat ( &iostat1, &iostat2, TRUE, iostat,
			       err, end, (LABEL_IDX) 0, zero_escape_freq );
	      st = Get_IoStruct_ST ( block, FID_CILIST, TRUE );
	      Gen_Io_PutFieldConst ( block, st, FSC_CIERR, errstat );
	      Gen_Io_PutFieldWN ( block, st, FSC_CIUNIT, unit_wn );
	      if (mp_io) {
		unit_ptr = Get_UnitPointer_ST();
		GEN_IO_CALL_2 ( block, FIO_EXT_REWRITE_LIST_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ), 
			      Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_LIST_start, iostat1, 
			      NULL, Make_IoAddr_WN ( st ) );
	      lower_io_items ( block, tree, FFT_LIST, iostat1,
			       iolist, WN_kid_count(tree) );
	      if (mp_io) {
		GEN_IO_CALL_1 ( block, FIO_EXT_REWRITE_LIST_end, iostat1, 
			      iostat2, Make_IoAddr_WN ( unit_ptr ) );
	      }
	      else
	        GEN_IO_CALL_0 ( block, FIO_EXT_REWRITE_LIST_end, iostat1,
			      iostat2 );
	      break;

	  default:
	      Fail_FmtAssertion(
			"unexpected I/O statement (%d) in I/O processing",
			ioclass);

	}
      } else {
    /* Calvin TODO */
      fprintf( stderr, "REWRITE not yet implemented\n" );
      abort();
      }
	
	break;

    default:
	Fail_FmtAssertion("unexpected I/O statement (%d) in I/O processing",
			  iostatement);

  }

  /*  Set the line number of all the generated (or reorganized) statements
      equal to the line number of the original I/O statement.  This post-
      processing is more efficient than passing the srcpos down through all
      support routines so that the line number can be set when the statement
      nodes are first created (or moved).  */

  srcpos = WN_Get_Linenum ( tree );
  for (wn = WN_first( block ); wn; wn = WN_next( wn )) {
    WN_Set_Linenum ( wn, srcpos );
  }

  if ( Cur_PU_Feedback ) {

    // Minimal feedback updating: WN_IO is instrumented like a CALL;
    //   if the WN_IO has in_out_same == true, then all CALLs produced
    //   during lowering should also have in_out_same set to true

    if ( Cur_PU_Feedback->Same_in_out( tree )  )
      Cur_PU_Feedback->FB_set_in_out_same( block );

    Cur_PU_Feedback->Delete( tree );
  }

  /*  We're all done with the top-level OPC_IO node so delete it.  */

  WN_Delete ( tree );

  /*  Almost done.  The block of statements representing the I/O
      statement has been created.  However; if other lowering is going
      on, it hasn't been applied to this block of statements.  So the
      block of statements just created need to be lowered before
      returning.  For efficiency, only do this if other lowering is
      going on and don't bother to specify IO_STATEMENT lowering, it's
      already been done.  Note that it is the responsibility of the
      caller to splice the returned block of statements into the
      enclosing block.  */

  if (actions != LOWER_IO_STATEMENT)
    block = lower_block ( block, actions & ~LOWER_IO_STATEMENT);

  return (block);

}


#define READ_WRITE_MASK 1
#define NAMELIST_MODE(x) ((x) & NML_MASK)
#define READ_MODE(x) ((x) & READ_WRITE_MASK)
#define WRITE_MODE(x) (!((x) & READ_WRITE_MASK))


/*
  Structures to associate dummy arguments with
  an entry point ST. There doesn't happen to be
  any association with the procedure definition in
  the PDGCS interface, and a little is needed for WN & dwarf.

  - Declared_args is number written in the source
  - Total_args includes hidden arguments eg: char lengths.
  - Args_seen - number of arg slots filled so far
  - Arg_lengths_seen - # of character length arguments, but
                       not including length of character function result.
  - Last_parm_ty_seen - TYLIST entry corresponding to Args_seen
                      on TEXT TY for procedure.
  - Last_len_ty_seen  - ditto for character lengths.
  - global - is global ST.
  - rty -  function result TY, for those which pass result as dummy
           and are marked as subroutine.

*/

typedef struct alist {
                INT32   total_args  ;
                INT32   declared_args  ;
                INT32   args_seen   ;
                INT32   arg_lengths_seen ;
                TYLIST  *last_parm_ty_seen  ;
                TYLIST  *last_len_ty_seen   ;
                struct alist  * next_entry ;
                TY      * rty      ;
                ST    **arglist    ;
                BOOL    global     ;
} DUMMIES ;


/*
    structures to associate a list of ST's with a parent
    ST, eg: the elements of a COMMON with the common ST,
    or alternate entry points of a procedure.
    Just for DST information.
*/

typedef struct blist {
                ST * element ;
                struct blist * next ;
} ITEM ;



typedef struct clist {
                ITEM * first  ;
                ITEM * last   ;
                INT32  nitems ;
} LIST ;


/*
  This defines an auxiliary struct for associating
  odds & ends with an ST. It's pointed to by the ST_temp
  field, and allocated in cwh_stab.c

 - dummy:   is a list of dummy args for a TEXT entry ST
 - pragma:  is a WN pragma - host variables of internal procs use them.
 - stptr:   pointer back to the ST, to clean up.
 - fstarg:  TRUE if function requires extra actual for result
 - isalt:   TRUE if this is an alternate entry point (DST)
 - altentry:list of alternate entry point STs (DST)
 - comlist: list of STs of elements of common block, associated with COMMON ST.
(DST)
 - nlist:   list of STs which comprise this namelist.
 - stem:    ST's DST name if internal  or module procedure
 - next:    pointer to next AUXST, to clean up.
 - pos:     declaration coordinates for this sym.
*/

typedef struct auxst {
        DUMMIES * dummy    ;
        WN      * pragma   ;
        ST      * stptr    ;
        BOOL      fstarg   ;
        BOOL      isalt    ;
        LIST      altentry ;
        LIST      comlist  ;
        LIST      nlist    ;
        char    * stem     ;
        struct auxst * next;
        USRCPOS   pos      ;
        INT32     assign_id;
        BOOL      visited;
} AUXST ;

static AUXST * Top_Auxst[INTERNAL_LEVEL+1] = {NULL,NULL,NULL,NULL};


/*===================================================
 *
 * cwh_stab_find_auxst
 *
 * Allocate or find field which holds details
 * associated with this ST. The ST_temp holds the
 * pointer. The create flag builds auxst field, if absent.
 *
 ====================================================
*/
static AUXST *
cwh_stab_find_auxst(ST *st, BOOL create)
{
  AUXST * o ;

  o = (AUXST *) BE_ST_io_auxst(ST_st_idx(st));

  if (o == NULL) {
    if (create) {
      o = (AUXST *) malloc(sizeof(AUXST));

      o ->dummy  = NULL;
      o ->pragma = NULL;
      o ->stptr  = st  ;
      o ->fstarg = FALSE;
      o ->isalt  = FALSE;
      o ->stem   = NULL;
      o ->next   = Top_Auxst[ST_level(st)];
      USRCPOS_clear(o->pos);
      o->assign_id = -1;
      o->visited = FALSE;

      o ->altentry.first = NULL;
      o ->altentry.last = NULL;
      o ->comlist.first = NULL;
      o ->comlist.last  = NULL;
      o ->nlist.first = NULL;
      o ->nlist.last  = NULL;

      BE_ST_set_io_auxst(ST_st_idx(st),(void *)o);
      Top_Auxst[ST_level(st)] = o ;
    }
  }
  return(o);
}


/*===================================================
 *
 * cwh_stab_visited
 *
 * Returns the address of the visited field in AUXST.
 *
 *
 ====================================================
*/
extern BOOL *
cwh_stab_visited(ST * st)
{
  AUXST *o ;

  o = cwh_stab_find_auxst(st, TRUE);
  return &(o->visited) ;
}


/*===================================================
 *
 * cwh_stab_free_list
 *
 * Free a LIST of items. Clears the LIST pointer.
 *
 ====================================================
*/
static void
cwh_stab_free_list (LIST ** lp)
{
  ITEM *i;
  ITEM *n;
  LIST *l;

  if (*lp != NULL) {
    l = *lp ;

    i = l->first ;

    while (i != NULL) {
      n = i-> next ;
      free(i) ;
      i = n ;
    }

    *lp = NULL ;
  }
}


/*===================================================
 *
 * cwh_stab_free_auxst
 *
 * Free any AUXSTs, and set the corresponding
 * ST_temp to NULL;
 *
 ====================================================
*/
static void
cwh_stab_free_auxst(void)
{
  AUXST *o,*n;
  LIST  *l   ;

  o = Top_Auxst[CURRENT_SYMTAB];

  while (o != NULL ) {

    BE_ST_set_io_auxst(ST_st_idx(o->stptr),NULL);

    l = &o->comlist;
    cwh_stab_free_list(&l);

    l = &o->altentry;
    cwh_stab_free_list(&l);

    l = &o->nlist;
    cwh_stab_free_list(&l);

    if (o->stem != NULL)
      free (o->stem) ;

    n = o->next ;
    free(o);
    o = n;
  }
  Top_Auxst[CURRENT_SYMTAB] = NULL;
}



/*===================================================
 *
 * cwh_io_ST_base
 *
 * Input : an ST
 * Output:  Base of the ST
 *
 *====================================================
*/

static ST *
cwh_io_ST_base(ST *st)
{

  ST *base;
  INT64 ofst;
  Expand_ST_into_base_and_ofst(st,ST_ofst(st),&base,&ofst);
  return base;
}




/*===================================================
 *
 * cwh_io_analyse_io_item
 *
 * Analyses one IO item to see if this item requires
 * that multiple calls be generated to the library. 
 * Returns FALSE if a single call will suffice; returns
 * TRUE otherwise. 
 * 
 * It uses the visited field in AUXST to analyse dependency
 * information.
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_io_item(WN *tree, IMPDO_INFO *impdo_set, INT32 mode)
{

   INTRINSIC item;
   WN *kid0;
   OPERATOR opr;
   ST *index;
   IMPDO_INFO *new_impdo_set;
   BOOL *visited;
   INT32 i;
   INT32 nd;

   item = (INTRINSIC) WN_intrinsic(tree);
   
   switch(item) {

   case IOL_IMPLIED_DO:

       index = cwh_io_ST_base(WN_st(WN_index(tree)));
       /* Don't create animplied-do entry for non-standard size do loop
       ** variable as the implied-do entry is not yet set up to handle
       ** them and so could cause bus error or incorrect result
       */
       if (ST_btype(WN_st(WN_index(tree))) != MTYPE_I4)
	 return TRUE;
       new_impdo_set = (IMPDO_INFO *) malloc(sizeof(IMPDO_INFO));
       Impdo_index(new_impdo_set) = WN_st(WN_index(tree));
       Impdo_next(new_impdo_set) = impdo_set;
       
       visited = cwh_stab_visited(index);
       if (*visited) {
          return TRUE; 
       } else {
          *visited = TRUE;
          cwh_io_add_st_to_marked_set(index);
       }

       if (cwh_io_analyse_expr(WN_start(tree), new_impdo_set, mode))
          return TRUE;
       else if (cwh_io_analyse_expr(WN_end(tree), new_impdo_set, mode))
          return TRUE;
       else if (cwh_io_analyse_expr(WN_step(tree), new_impdo_set, mode))
          return TRUE;
        
       for(i=4; i<WN_kid_count(tree); i++) {
          if (cwh_io_analyse_io_item(WN_kid(tree,i), new_impdo_set, mode))
             return TRUE;
	  /* For anything related to a structures 
	  ** inside an implied-do generate a loop */
	  if ((WN_io_item(WN_kid(tree,i)) == IOL_VAR 
		 || WN_io_item(WN_kid(tree,i)) == IOL_EXPR)
	      &&TY_kind(WN_ty(WN_kid(tree,i))) == KIND_STRUCT)
	    return TRUE;
          if (WN_io_item(WN_kid(tree,i)) == IOL_ARRAY) {
	    for (kid0 = WN_kid0(WN_kid(tree,i)); WNOPR(kid0) != OPR_LDA; )
	      kid0 = WN_kid0(kid0);
	    if (TY_kind(TY_pointed(WN_ty(kid0))) == KIND_ARRAY
	      && TY_kind(TY_AR_etype(TY_pointed(WN_ty(kid0)))) == KIND_STRUCT)
	      return TRUE;
	  }
       }
       break; 
       
   case IOL_EXPR:
   case IOL_LOGICAL:

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if (opr == OPR_ILOAD && WNOPR(WN_kid0(kid0)) == OPR_ARRAY) {
          if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
             return TRUE;
       } else if (opr == OPR_CVTL && WN_kid_count(WN_kid0(kid0)) > 0 && WNOPR(WN_kid0(WN_kid0(kid0))) == OPR_ARRAY) {
          if (cwh_io_analyse_arr(WN_kid0(WN_kid0(kid0)), impdo_set, mode))
             return TRUE;
       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_VAR:

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {
          if (cwh_io_analyse_arr(kid0, impdo_set, mode))
             return TRUE;
       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_CHAR:
       kid0 = WN_kid0(tree);

       /* If the length is dependent on some variable, split */

       if (cwh_io_analyse_expr(WN_kid1(tree), impdo_set, mode))
	  return TRUE;

       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {

          nd = WN_kid_count(kid0)/2;	

	  if (WNOPR(WN_kid0(kid0)) == OPR_LDA) {
             if (cwh_io_analyse_arr(kid0, impdo_set, mode))
                return TRUE;
          } else if ((nd == 1) && (WNOPR(WN_kid0(kid0)) == OPR_ARRAY)) {
	     if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
		return TRUE;
             for (i=2*nd; i > nd; i-- ) {
                if (cwh_io_analyse_index_expr(WN_kid(kid0,i), 
                                           impdo_set, mode) != 0)
                return TRUE;
             }
          } else {
             if (cwh_io_analyse_expr(kid0, impdo_set, mode))
                return TRUE;
          } 

       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_DOPE:
       /* not applicable to F77 */
   case IOL_ARRAY:
       if (WN_kid_count(tree) > 0
	   && WNOPR(WN_kid0(tree)) == OPR_LDA
	   && TY_kind(WN_ty(WN_kid0(tree))) == KIND_POINTER
	   && TY_kind(TY_pointed(WN_ty(WN_kid0(tree)))) == KIND_ARRAY
	   && TY_kind(TY_AR_etype(TY_pointed(WN_ty(WN_kid0(tree))))) == KIND_STRUCT)
	 return TRUE;
   case IOL_CHAR_ARRAY:
       break;
	 
   default:
       DevAssert((0),("Odd iolist Item"));
   }

   return FALSE;
}
        

/*===================================================
 *
 * cwh_io_analyse_expr
 *
 * Analyse any expression to see if any ST in that
 * expression has the visited bit set, and if so,
 * return TRUE to indicate that the IO statement this
 * expression appears in, needs multiple calls.
 *
 * If an ST is encountered that does not have visited
 * bit set, the bit is now set so that dependency with
 * any subsequent expressions can be analyzed.
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_expr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode)
{
   ST *st;
   BOOL *visited;
   INT32 i;
  
   if ( OPCODE_has_aux(WN_opcode(tree))) {
     st = cwh_io_ST_base(WN_st(tree));
     visited = cwh_stab_visited(st);
     if (*visited) {
        return TRUE;
     } else if (READ_MODE(mode)) {
        *visited = TRUE;
        cwh_io_add_st_to_marked_set(st);
     }
   } else {
     for ( i = 0; i < WN_kid_count(tree); i++ )
       if (cwh_io_analyse_expr(WN_kid(tree,i), impdo_set, mode))
          return TRUE;
   }
   return FALSE;
}

/*===================================================
 *
 * cwh_io_analyse_arr
 *
 * Analyse an OPC_ARRAY node or an OPC_ARRSECTION node
 * to see if it has constructs that prevent a single call
 * from being generated. If kid0 is anything other than 
 * an LDA, a TRUE value is returned to indicate multiple
 * calls are required. The indexes along all dimensions 
 * are analyzed (see cwh_io_analyse_index_expr) and if
 * everything looks okay a value of FALSE is returned
 * to indicate that this node does not prevent a single call
 * from being generated.
 *
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_arr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode)
{
  INT32 nd;
  WN *addr;
  ST *st;
  BOOL *visited;
  INT32 i;

  nd = WN_kid_count(tree)/2;

  /* addr = cwh_addr_find_address(tree); */
  addr = WN_kid0(tree);

  if (WNOPR(addr) == OPR_LDA) {

     st = cwh_io_ST_base(WN_st(addr));
     if (ST_class(st) == CLASS_BLOCK) /* PV 572913, don't know how to handle these */
	return TRUE;
     if (TY_kind(ST_type(st)) == KIND_STRUCT)
        return TRUE;
     visited = cwh_stab_visited(st);
     if (*visited) {
        return TRUE;
     } else if (READ_MODE(mode)) {
        *visited = TRUE;
        cwh_io_add_st_to_marked_set(st);
     }
     for (i=2*nd; i > nd; i-- ) {
         if (cwh_io_analyse_index_expr(WN_kid(tree,i), impdo_set, mode) == -1)
            return TRUE;
     }
  } else {
     if (cwh_io_analyse_expr(tree, impdo_set, mode))
        return TRUE;
  }

  return FALSE;
}

/*===================================================
 *
 * cwh_io_analyse_index_expr
 *
 * Analyze the index along a dimension of an ARRAY or
 * ARRSECTION node. If the index expr happens to be
 * an OPR_TRIPLET, then the kids representing the extent
 * and stride are analyzed by cwh_io_analyse_expr to make 
 * sure there is no dependency on an ST that has visited 
 * bit set. kid0 of the TRIPLET node is analyzed by this
 * same routine as would an index under an OPC_ARRAY node.
 *
 * If the index is of the form <i+<expr>> or <i-<expr>>
 * where i can possible be an implied do index, and expr
 * does not contain an implied do index, then it is a 
 * candidate for a single call. If anything in <expr> 
 * has the visited bit set, multiple calls are needed.
 * Any expressions which is not of the above type and 
 * is not a constant will cause a multiple call to be 
 * generated.
 *
 *====================================================
*/

static INT32
cwh_io_analyse_index_expr(WN *tree, IMPDO_INFO *impdo_set, INT32 mode)
{
   INT32 kid0_status;
   INT32 kid1_status;
   INT32 i;
   INT32 pos;
   BOOL *visited;
   ST *st;

   if (WNOPR(tree) == OPR_TRIPLET) {
     for (i=1; i<=2; i++) 
        if (cwh_io_analyse_expr(WN_kid(tree, i), impdo_set, mode))
           return -1;

     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, 
                                             mode);
     return kid0_status;
     
   } else if (WNOPR(tree) == OPR_LDID) {

      if ( (pos = member (WN_st(tree), impdo_set)) != 0) {
         return pos;
      } else {
         st = cwh_io_ST_base(WN_st(tree));
         visited = cwh_stab_visited(st);
         if (*visited)
           return -1;
         else
           return 0;
      }
   } else if ( WN_operator_is(tree,OPR_CONST) ||
               WN_operator_is(tree,OPR_INTCONST)) {
      return 0;
   } else if ( WN_operator_is(tree,OPR_ADD) ) { 
     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, mode);
     kid1_status = cwh_io_analyse_index_expr(WN_kid1(tree), impdo_set, mode);
     switch(kid0_status) {
       case 0:
          return kid1_status;
       case -1:
          return -1;
       default: /* >= 1 */
          if (kid1_status == 0)
             return kid0_status;
          else
             return -1;
     }
   } else if (WN_operator_is(tree,OPR_SUB)) { 
     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, mode);
     kid1_status = cwh_io_analyse_index_expr(WN_kid1(tree), impdo_set, mode);
     switch(kid0_status) {
       case 0:
	  if (kid1_status == 0)
	     return 0;
          else
	     return -1;
       case -1:
          return -1;
       default: /* >= 1 */
          if (kid1_status == 0)
             return kid0_status;
          else
             return -1;
     }
   }
   return -1;
}

/*===================================================
 *
 * member
 *
 * Checks if the given ST is a memeber of the ST's
 * contained in the implied do set. If it is a member,
 * the position of the ST is returned, else 0 is returned.
 *
 *
 *====================================================
*/

static mINT32
member(ST *st, IMPDO_INFO *impdo_set)
{
  mINT32 ret_val = 1;
  while (impdo_set) {
     if (st == Impdo_index(impdo_set))
        return ret_val;
     impdo_set = Impdo_next(impdo_set);
     ret_val++;
  }
  return 0;
}


/*===================================================
 *
 * cwh_io_search_implied_do_index
 *
 * Checks if any ST in the array index <passed in tree>
 * is a member of the implied do index set. If yes, the 
 * position of the ST is returned, else, 0 is returned.
 *
 *====================================================
*/

static INT32 
cwh_io_search_implied_do_index(WN *tree, IMPDO_INFO *impdo_set)
{
  INT32 pos;
  ST *st;
  INT32 i;

  if (WNOPR(tree) == OPR_LDID) {
     st = WN_st(tree);
     if ( (pos = member (st, impdo_set)) != 0) 
        return pos;
  } else {
     for(i=0; i < WN_kid_count(tree); i++) {
        pos = cwh_io_search_implied_do_index(WN_kid(tree, i),impdo_set);
	if (pos != 0)
           return pos;
     }
  }
  return 0;
}

/*===================================================
 *
 * cwh_io_add_st_to_marked_set
 *
 * Adds a new ST to the visited set.
 *
 *====================================================
*/

static void
cwh_io_add_st_to_marked_set(ST *st) {
 
   MARKED_SET *new_marked_set;
 
   new_marked_set = (MARKED_SET *) malloc(sizeof(MARKED_SET));
   Marked_st(new_marked_set)  = st;
   Marked_next(new_marked_set) = marked_set;

   marked_set = new_marked_set; 
}

/*===================================================
 *
 * cwh_io_unmark
 *
 * Unmarks all ST's that were marked visited, since we
 * are done with this IO statement.
 *
 *====================================================
*/
 
static void 
cwh_io_unmark(void) {

   BOOL *visited;
   MARKED_SET *temp;

   while(marked_set)  {
      temp = marked_set;
      visited = cwh_stab_visited(Marked_st(marked_set));
      *visited = FALSE;
      marked_set = Marked_next(marked_set);
      free(temp);
   }
}
      
/*===================================================
 *
 * Substitute_1_For_Impdo_Index_Val
 *
 * Searches for occurences of the implied do index in
 * the tree, and replaces the occurence by an INTCONST
 * node with val 1.
 *
 ====================================================
*/

static WN *
Substitute_1_For_Impdo_Index_Val(WN *tree, IMPDO_INFO *impdo)
{
  INT32 i;
  OPCODE opc_intconst;
  INT32 rtype;

  if (WN_operator_is(tree,OPR_LDID) && 
      (WN_st(tree) == Impdo_index(impdo)) ) {
     rtype = WN_rtype(tree);
     switch (rtype) {
       case MTYPE_I4:
         opc_intconst = OPC_I4INTCONST;
         break;
       case MTYPE_I8:
         opc_intconst = OPC_I8INTCONST;
         break;
       case MTYPE_U4:
         opc_intconst = OPC_U4INTCONST;
         break;
       case MTYPE_U8:
         opc_intconst = OPC_U8INTCONST;
         break;
       default:
          DevAssert((0),("Odd type"));
     }
     return (WN_CreateIntconst ( opc_intconst, 1));
  } else {
     for(i=0; i<WN_kid_count(tree); i++ ) {
       WN_kid(tree, i) = Substitute_1_For_Impdo_Index_Val(WN_kid(tree, i),
                                                          impdo);
     }
  }
  return tree;
}

/*===================================================
 *
 * OPCODE_has_aux
 *
 * Return TRUE if operator is an LDID, or LDA or STID.
 *
 *====================================================
*/

static BOOL 
OPCODE_has_aux(const OPCODE opc)
{

  OPERATOR opr = OPCODE_operator(opc);
  return (opr == OPR_LDID || opr == OPR_STID || 
	  opr == OPR_LDA || opr == OPR_IDNAME);
}

// lower_io_init should be called at least once per region.
extern void
Lower_IO_Init (void)
{
  if (Current_pu != cray_iolist_current_pu) {
     cray_iolist_current_pu = Current_pu;
     if (stack_ty == (TY_IDX) 0)
    	 stack_ty = Make_Simple_Array_Type("stack_space_type", STACK_LENGTH,
                                                Be_Type_Tbl(MTYPE_U8));
     stack_st = Gen_Temp_Symbol ( stack_ty, TY_name(stack_ty) );
     io_set_addr_passed_flag(stack_st);
  }
  container_block_for_iolists = NULL;	// so not unioned with previous blocks
  num_iolists = 0;

  // clear out IoStruct_ST
  if (Current_pu != fio_current_pu) {
    fio_current_pu = Current_pu;
    local_sequence = 1;
    INT32 i;
    for (i=FIOSTRUCTID_FIRST; i<=FIOSTRUCTID_LAST; i++)
      fiostruct_st[i] = NULL;
    cilist_st = NULL;
  }

}


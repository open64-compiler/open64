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



#ifndef __UNWIND_PRODUCER_H
#define __UNWIND_PRODUCER_H

/* unwind descriptor record formats */
typedef struct __unw_format_r1_struct {
        char _fix[1];
} __unw_format_r1_t;
typedef struct __unw_format_r2_struct {
        char _fix[2];
        char _var[1];
} __unw_format_r2_t;
typedef struct __unw_format_r3_struct {
        char _fix[1];
        char _var[1];
} __unw_format_r3_t;
typedef struct __unw_format_p1_struct {
        char _fix[1];
} __unw_format_p1_t;
typedef struct __unw_format_p2_struct {
        char _fix[2];
} __unw_format_p2_t;
typedef struct __unw_format_p3_struct {
        char _fix[2];
} __unw_format_p3_t;
typedef struct __unw_format_p4_struct {
        char _fix[1];
        char _var[1];
} __unw_format_p4_t;
typedef struct __unw_format_p5_struct {
        char _fix[4];
} __unw_format_p5_t;
typedef struct __unw_format_p6_struct {
        char _fix[1];
} __unw_format_p6_t;
typedef struct __unw_format_p7_struct {
        char _fix[1];
        char _var[1];
} __unw_format_p7_t;
typedef struct __unw_format_p8_struct {
        char _fix[2];
        char _var[1];
} __unw_format_p8_t;
typedef struct __unw_format_p9_struct {
        char _fix[3];
} __unw_format_p9_t;
typedef struct __unw_format_p10_struct {
        char _fix[3];
} __unw_format_p10_t;
typedef struct __unw_format_b1_struct {
        char _fix[1];
} __unw_format_b1_t;
typedef struct __unw_format_b2_struct {
        char _fix[1];
        char _var[1];
} __unw_format_b2_t;
typedef struct __unw_format_b3_struct {
        char _fix[1];
        char _var[1];
} __unw_format_b3_t;
typedef struct __unw_format_b4_struct {
        char _fix[1];
        char _var[1];
} __unw_format_b4_t;
typedef struct __unw_format_x1_struct {
        char _fix[2];
        char _var[2];
} __unw_format_x1_t;
typedef struct __unw_format_x2_struct {
        char _fix[3];
        char _var[1];
} __unw_format_x2_t;
typedef struct __unw_format_x3_struct {
        char _fix[3];
        char _var[2];
} __unw_format_x3_t;
typedef struct __unw_format_x4_struct {
        char _fix[4];
        char _var[1];
} __unw_format_x4_t;

/* unwind table space */
#define __UNW_TABLE_ENTRIES_SIZE        0x80
extern __unw_table_entry_t *_unwind_table;
extern __uint64_t _unwind_table_total_size;
extern __uint64_t _unwind_table_size;
/* current unwind table entry space */
extern __unw_table_entry_t _current_unwind_table_entry;

/* unwind info space */
#define __UNW_INFO_SIZE                 0x100000
extern __unw_info_t *_unwind_info;
extern __uint64_t _unwind_info_total_size;
extern __uint64_t _unwind_info_size;
/* current unwind info space */
#define __UNW_CURRENT_INFO_SIZE         0x400
extern __unw_info_t *_current_unwind_info;
extern __uint64_t _current_unwind_info_total_size;
extern __uint64_t _current_unwind_info_size;

/* current procedure and current frame info */
extern __uint64_t _current_procedure_size; /* measured in instruction slots */
extern __uint64_t _current_procedure_total_size; /* measured in instruction slots */
extern __uint64_t _current_region_total_size; /* measured in instruction slots */
extern __uint32_t _current_region_id;
enum { __UNW_UNDEF, __UNW_PROLOGUE, __UNW_BODY };

/* uhandler and ehandler flags */
extern __uint32_t _ehandler;
extern __uint32_t _uhandler;

/* personality routine gp-relative offset */
extern __unw_addr_t _personality;

/* language-specific data */
#define __UNW_LANG_SPEC_SIZE		0x400
extern void *_lang_spec_data;
extern __uint64_t _lang_spec_data_size;
extern __uint64_t _lang_spec_data_total_size;

/* imask data */
extern void *_imask;
extern __uint64_t _imask_size;
extern __uint64_t _imask_total_size;

/* encoding space size */
#define __UNW_ENCODING_SIZE             0x50

/* prototypes */
__unw_error_t unwind_info_add_desc(__uint64_t, char *);
__unw_error_t unwind_info_set_imask(__unw_info_t *, __uint32_t,
							__uint64_t);
__unw_error_t unwind_info_get_imask(__unw_info_t *, __uint32_t *,
							__uint64_t);
__unw_error_t unwind_info_add_imask(__unw_info_t *);


/* enum of all regs that can be spilled */
typedef enum {
	__UNW_R4,
	__UNW_R5,
	__UNW_R6,
	__UNW_R7,
	__UNW_F2,
	__UNW_F3,
	__UNW_F4,
	__UNW_F5,
	__UNW_F16,
	__UNW_F17,
	__UNW_F18,
	__UNW_F19,
	__UNW_F20,
	__UNW_F21,
	__UNW_F22,
	__UNW_F23,
	__UNW_F24,
	__UNW_F25,
	__UNW_F26,
	__UNW_F27,
	__UNW_F28,
	__UNW_F29,
	__UNW_F30,
	__UNW_F31,
	__UNW_B1,
	__UNW_B2,
	__UNW_B3,
	__UNW_B4,
	__UNW_B5,
	__UNW_PRED,
	__UNW_PSP,
	__UNW_PRIUNAT,
	__UNW_RP,
	__UNW_BSP,
	__UNW_BSPSTORE,
	__UNW_RNAT,
	__UNW_UNAT,
	__UNW_FPSR,
	__UNW_PFS,
	__UNW_LC,
	__UNW_UNDEFINED_REG
} __UNW_REG_TYPE;

#endif

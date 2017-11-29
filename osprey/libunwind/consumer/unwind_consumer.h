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



#ifndef __UNWIND_CONSUMER_H
#define __UNWIND_CONSUMER_H

/* unwind consumer verbose environment variable */
#define __UNW_CONSUMER_VERBOSE_ENV_VAR "_UNW_VERBOSE"
extern __uint32_t _unwind_verbose;
#define __UNW_VERBOSE_ERRORS 1
#define __UNW_VERBOSE_WARNINGS 2
#define __UNW_VERBOSE_MSGS 3
#define __UNW_VERBOSE_INTERNAL_MSGS 4

/* unwind table space */
extern __unw_table_entry_t *_unwind_table;
extern __uint64_t _unwind_table_size;

/* unwind info space */
extern __unw_info_t *_unwind_info;
extern __uint64_t _unwind_info_size;

/* register restore info type */
#define __UNW_UNINITIALIZED 0
#define __UNW_NO_RESTORE 1
#define __UNW_RESTORE_OFF_GR 2
#define __UNW_RESTORE_OFF_BR 3
#define __UNW_RESTORE_SP_RELATIVE 4
#define __UNW_RESTORE_PSP_RELATIVE 5
#define __UNW_TO_RESTORE_PSP_RELATIVE 6
#define __UNW_RESTORE_FIXED_VALUE 7
typedef struct __unw_reg_info_struct {
	__uint32_t _code : 3;
	__uint32_t _reg : 29;
	__uint64_t _offset;
} __unw_reg_info_t;

/* state restore info type */
#define __UNW_MAX_GR_PRESERVED 5
#define __UNW_GR_STD_START 0
#define __UNW_GR_STD_END 3
#define __UNW_GR4 0
#define __UNW_GR5 1
#define __UNW_GR6 2
#define __UNW_GR7 3
#define __UNW_SP 4
#define __UNW_MAX_FR_PRESERVED 20
#define __UNW_FR_STD_START 0
#define __UNW_FR_STD_END 19
#define __UNW_FR_LOW_START 0
#define __UNW_FR_LOW_END 3
#define __UNW_FR_HIGH_START 4
#define __UNW_FR_HIGH_END 19
#define __UNW_FR2 0
#define __UNW_FR3 1
#define __UNW_FR4 2
#define __UNW_FR5 3
#define __UNW_FR16 4
#define __UNW_FR17 5
#define __UNW_FR18 6
#define __UNW_FR19 7
#define __UNW_FR20 8
#define __UNW_FR21 9
#define __UNW_FR22 10
#define __UNW_FR23 11
#define __UNW_FR24 12
#define __UNW_FR25 13
#define __UNW_FR26 14
#define __UNW_FR27 15
#define __UNW_FR28 16
#define __UNW_FR29 17
#define __UNW_FR30 18
#define __UNW_FR31 19
#define __UNW_MAX_BR_PRESERVED 6
#define __UNW_BR_STD_START 1
#define __UNW_BR_STD_END 5
#define __UNW_RP 0
#define __UNW_BR1 1
#define __UNW_BR2 2
#define __UNW_BR3 3
#define __UNW_BR4 4
#define __UNW_BR5 5
#define __UNW_MAX_AR_PRESERVED 7

#define __UNW_BSP 0
#define __UNW_BSPSTORE 1
#define __UNW_RNAT 2
#define __UNW_UNAT 3
#define __UNW_FPSR 4
#define __UNW_PFS 5
#define __UNW_LC 6

typedef struct __unw_state_info_struct {
	__unw_reg_info_t _gr[__UNW_MAX_GR_PRESERVED];
	__unw_reg_info_t _fr[__UNW_MAX_FR_PRESERVED];
	__unw_reg_info_t _br[__UNW_MAX_BR_PRESERVED];
	__unw_reg_info_t _ar[__UNW_MAX_AR_PRESERVED];
	__unw_reg_info_t _preds;
	__unw_reg_info_t _priunat;
	__uint64_t _frame_size;
	__uint64_t _spill_base;
	__uint64_t _spill_offset;
	__uint64_t _spill_order;
	__uint64_t _label;
} __unw_state_info_t;

/* unwind state stack space */
#define __UNW_STATE_STACK_ENTRIES_SIZE 0x80
extern __unw_state_info_t *_unwind_state_stack;
extern __uint64_t _unwind_state_stack_total_size;
extern __uint64_t _unwind_state_stack_size;

/* unwind register mappings from state info to real registers */
extern const __uint32_t _unw_gr_map[__UNW_MAX_GR_PRESERVED];
extern const __uint32_t _unw_fr_map[__UNW_MAX_FR_PRESERVED];
extern const __uint32_t _unw_br_map[__UNW_MAX_BR_PRESERVED];
extern const __uint32_t _unw_ar_map[__UNW_MAX_AR_PRESERVED];

/* context access macros */
#define __UNW_CONTEXT_ACCESS_GR(x,i) (((x)->context.sc_gr)[i])
#define __UNW_CONTEXT_ACCESS_FR(x,i) (((x)->context.sc_fr)[i])
#define __UNW_CONTEXT_ACCESS_BR(x,i) (((x)->context.sc_br)[i])

#define __UNW_AR_MAP(i)              (_unw_ar_map[(i)])
#define __UNW_CONTEXT_ACCESS_AR(x,i) *(((uint64_t*) &((x)->context.sc_ar_bsp) + i))
#define __UNW_CONTEXT_ACCESS_AR_EC(x) ((x)->sc_ar_ec)

#define __UNW_CONTEXT_ACCESS_IP(x) ((x)->context.sc_ip)
#define __UNW_CONTEXT_ACCESS_GP(x) (((x)->context.sc_gr)[1])
#define __UNW_CONTEXT_ACCESS_PREDS(x) (((x)->context.sc_pr))
#define __UNW_CONTEXT_ACCESS_PRIUNAT(x) ((x)->sc_priunat)

/* XXX temporary sigcontext_t definition XXX
typedef struct sigcontext_struct {
	__uint64_t _gr[128];
	long double _fr[128];
	__uint64_t _br[8];
	__uint64_t _ar[128];
	__uint64_t _ip;
	unsigned char _preds[64];
	__uint64_t _priunat;
} sigcontext_t; */

/* prototypes */
__unw_error_t unwind_state_stack_reset(void);
__unw_error_t unwind_state_stack_pop(__uint64_t);
__unw_error_t unwind_state_stack_push(__unw_state_info_t **);
__unw_error_t unwind_state_stack_top(__unw_state_info_t **);
__unw_error_t unwind_state_stack_search(__unw_state_info_t **, __uint64_t);
__unw_error_t unwind_process_desc(__uint64_t, __unw_info_t *, __unw_state_info_t *);
__unw_error_t unwind_output(char *, ...);

#endif

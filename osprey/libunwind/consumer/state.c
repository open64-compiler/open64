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



#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <strings.h>
#include <sys/unwind.h>
#include "unwind_consumer.h"



/* header unwind descriptor processing */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_process_header_desc(__unw_state_info_t **state,
					__uint64_t slots, __uint64_t total_slots,
					__uint64_t *rlen, char *ptr, __uint64_t size) {
	__unw_error_t ret;
	__uint64_t count = 0L, num, val;
	__uint32_t reg;
	unsigned char loc;

	if (__UNW_IS_R1(*ptr)) {
		count = 1;
		*rlen = (__uint64_t)((*ptr) & 0x1f);
		if (__UNW_OK != (ret = unwind_state_stack_push(state))) {
			return ret;
		}
	} else if (__UNW_IS_R2(*ptr)) {
		count = 2;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		*rlen = val;
		if (__UNW_OK != (ret = unwind_state_stack_push(state))) {
			return ret;
		}

		/* update state */
		if (slots >= total_slots + *rlen) {
			loc = (unsigned char)(*ptr & 0x07);
			loc <<= 1;
			loc |= ((*(ptr+1) & 0x80) >> 7);
			reg = (__uint32_t)(*(ptr+1) & 0x7f);
			if (loc & 0x08) {
				(*state)->_br[__UNW_RP]._code = __UNW_RESTORE_OFF_GR;
				(*state)->_br[__UNW_RP]._reg = reg++;
			}
			if (loc & 0x04) {
				(*state)->_ar[__UNW_PFS]._code = __UNW_RESTORE_OFF_GR;
				(*state)->_ar[__UNW_PFS]._reg = reg++;
			}
			if (loc & 0x02) {
				(*state)->_gr[__UNW_SP]._code = __UNW_RESTORE_OFF_GR;
				(*state)->_gr[__UNW_SP]._reg = reg++;
			}
			if (loc & 0x01) {
				(*state)->_preds._code = __UNW_RESTORE_OFF_GR;
				(*state)->_preds._reg = reg;
			}
		}
	} else if (__UNW_IS_R3(*ptr)) {
		count = 1;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		*rlen = val;
		if (__UNW_OK != (ret = unwind_state_stack_push(state))) {
			return ret;
		}
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_header_desc() ERROR: %s",
				"invalid header descriptor");
		}
	}
	return count;
}



/* prologue unwind descriptor processing */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_process_prologue_desc(__unw_state_info_t **state,
						__uint64_t slots, __uint64_t total_slots,
						__uint64_t rlen, char *ptr, __uint64_t size) {
	__uint64_t count = 0L, spill_order = 0L, ret, val, val1, val2, i, j;
	__uint32_t reg;
	unsigned char loc;

	if (__UNW_IS_P1(*ptr)) {
		count = 1;
		loc = (unsigned char)(*ptr & 0x1f);
		for (i = __UNW_BR_STD_START, j = 0; i <= __UNW_BR_STD_END; i++, j++) {
			if (loc & (0x1 << j)) {
				if ((*state)->_spill_base) {
					(*state)->_br[i]._reg = (__uint32_t)(*state)->_spill_order++;
					if (__UNW_NO_RESTORE != (*state)->_br[i]._code) {
						(*state)->_br[i]._code = __UNW_RESTORE_PSP_RELATIVE;
					}
					(*state)->_br[i]._offset = (*state)->_spill_base +
									(*state)->_spill_offset;
					(*state)->_spill_offset += sizeof(__uint64_t);
				} else {
					(*state)->_br[i]._reg = (__uint32_t)(*state)->_spill_order++;
					if (__UNW_NO_RESTORE != (*state)->_br[i]._code) {
						(*state)->_br[i]._code = __UNW_TO_RESTORE_PSP_RELATIVE;
					}
					(*state)->_br[i]._offset = (*state)->_spill_offset;
					(*state)->_spill_offset += sizeof(__uint64_t);
				}
			}
		}
	} else if (__UNW_IS_P2(*ptr)) {
		count = 2;
		loc = (unsigned char)(*ptr & 0x0f);
		loc <<= 1;
		loc |= ((*(ptr+1) & 0x80) >> 7);
		reg = (__uint32_t)(*(ptr+1) & 0x7f);
		for (i = __UNW_BR_STD_START, j = 0; i <= __UNW_BR_STD_END; i++, j++) {
			if (loc & (0x1 << j)) {
				if (__UNW_NO_RESTORE != (*state)->_br[i]._code) {
					(*state)->_br[i]._code = __UNW_RESTORE_OFF_GR;
				}
				(*state)->_br[i]._reg = reg++;
			}
		}
	} else if (__UNW_IS_P3(*ptr)) {
		count = 2;
		loc = (unsigned char)(*ptr & 0x07);
		loc <<= 1;
		loc |= ((*(ptr+1) & 0x80) >> 7);
		reg = (__uint32_t)(*(ptr+1) & 0x7f);
		switch (loc) {
		    case 0:
			if (__UNW_NO_RESTORE != (*state)->_gr[__UNW_SP]._code) {
				(*state)->_gr[__UNW_SP]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_gr[__UNW_SP]._reg = reg;
			break;
		    case 1:
			if (__UNW_NO_RESTORE != (*state)->_br[__UNW_RP]._code) {
				(*state)->_br[__UNW_RP]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_br[__UNW_RP]._reg = reg;
			break;
		    case 2:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_PFS]._code ) {
				(*state)->_ar[__UNW_PFS]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_PFS]._reg = reg;
			break;
		    case 3:
			if (__UNW_NO_RESTORE != (*state)->_preds._code) {
				(*state)->_preds._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_preds._reg = reg;
			break;
		    case 4:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_UNAT]._code) {
				(*state)->_ar[__UNW_UNAT]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_UNAT]._reg = reg;
			break;
		    case 5:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_LC]._code) {
				(*state)->_ar[__UNW_LC]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_LC]._reg = reg;
			break;
		    case 6:
			if (__UNW_NO_RESTORE != (*state)->_br[__UNW_RP]._code) {
				(*state)->_br[__UNW_RP]._code = __UNW_RESTORE_OFF_BR;
			}
			(*state)->_br[__UNW_RP]._reg = reg;
			break;
		    case 7:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_RNAT]._code) {
				(*state)->_ar[__UNW_RNAT]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_RNAT]._reg = reg;
			break;
		    case 8:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSP]._code) {
				(*state)->_ar[__UNW_BSP]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_BSP]._reg = reg;
			break;
		    case 9:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSPSTORE]._code) {
				(*state)->_ar[__UNW_BSPSTORE]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_BSPSTORE]._reg = reg;
			break;
		    case 10:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_FPSR]._code) {
				(*state)->_ar[__UNW_FPSR]._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_ar[__UNW_FPSR]._reg = reg;
			break;
		    case 11:
			if (__UNW_NO_RESTORE != (*state)->_priunat._code) {
				(*state)->_priunat._code = __UNW_RESTORE_OFF_GR;
			}
			(*state)->_priunat._reg = reg;
			break;
		    default:
			break;
		}
	} else if (__UNW_IS_P4(*ptr)) {
		__uint64_t desc_size = ((rlen * 2) + 7 ) / 8,
			byte_no = 0L, shift_no = 0L;

		count = 1;
		if (1 + desc_size > size) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_process_prologue_desc() ERROR: %s",
					"P4 processing failed with invalid size");
			}
			count += desc_size;
		} else {
			for (i = 0; i < (rlen * 2); i += 2) {
				byte_no = (i / 8) + 1;
				shift_no = i % 8;

				loc = (unsigned char)(*(ptr + byte_no));
				loc <<= shift_no;
				loc >>= 6;

				if (0x01 == loc) {
					for (j = __UNW_FR_STD_START; j <= __UNW_FR_STD_END; j++) {
						if (((__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_fr[j]._code) ||
								(__UNW_RESTORE_PSP_RELATIVE == (*state)->_fr[j]._code)) &&
								(spill_order == (*state)->_fr[j]._reg)) {
							if (slots <= i/2) {
								(*state)->_fr[j]._code = __UNW_NO_RESTORE;
								(*state)->_fr[j]._offset = 0L;
							}
							(*state)->_fr[j]._reg = 0;
							spill_order++;
						}
					}
				} else if (0x02 == loc) {
					for (j = __UNW_GR_STD_START; j <= __UNW_GR_STD_END; j++) {
						if (((__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_gr[j]._code) ||
								(__UNW_RESTORE_PSP_RELATIVE == (*state)->_gr[j]._code)) &&
								(spill_order == (*state)->_gr[j]._reg)) {
							if (slots <= i/2) {
								(*state)->_gr[j]._code = __UNW_NO_RESTORE;
								(*state)->_gr[j]._offset = 0L;
							}
							(*state)->_gr[j]._reg = 0;
							spill_order++;
						}
					}
				} else if (0x03 == loc) {
					for (j = __UNW_BR_STD_START; j <= __UNW_BR_STD_END; j++) {
						if (((__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_br[j]._code) ||
								(__UNW_RESTORE_PSP_RELATIVE == (*state)->_br[j]._code)) &&
								(spill_order == (*state)->_br[j]._reg)) {
							if (slots <= i/2) {
								(*state)->_br[j]._code = __UNW_NO_RESTORE;
								(*state)->_br[j]._offset = 0L;
							}
							(*state)->_br[j]._reg = 0;
							spill_order++;
						}
					}
				}

			}
			count += desc_size;
		}
	} else if (__UNW_IS_P5(*ptr)) {
		__uint32_t frmask = 0;

		count = 4;
		loc = (unsigned char)((*(ptr+1) & 0xf0) >> 4);
		for (i = __UNW_GR_STD_START, j = 0; i <= __UNW_GR_STD_END; i++, j++) {
			if (loc & (0x1 << j)) {
				if ((*state)->_spill_base) {
					(*state)->_gr[i]._reg = (__uint32_t)(*state)->_spill_order++;
					if (__UNW_NO_RESTORE != (*state)->_gr[i]._code ) {
						(*state)->_gr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
					}
					(*state)->_gr[i]._offset = (*state)->_spill_base +
									(*state)->_spill_offset;
					(*state)->_spill_offset += sizeof(__uint64_t);
				} else {
					(*state)->_gr[i]._reg = (__uint32_t)(*state)->_spill_order++;
					if (__UNW_NO_RESTORE != (*state)->_gr[i]._code) {
						(*state)->_gr[i]._code = __UNW_TO_RESTORE_PSP_RELATIVE;
					}
					(*state)->_gr[i]._offset = (*state)->_spill_offset;
					(*state)->_spill_offset += sizeof(__uint64_t);
				}
			}
		}
		frmask = (__uint32_t)(*(ptr+1) & 0x0f);
		frmask <<= 4;
		frmask |= *(ptr+2);
		frmask <<= 8;
		frmask |= *(ptr+3);
		for (i = __UNW_FR_STD_START, j = 0; i <= __UNW_FR_STD_END; i++, j++) {
			if (frmask & (0x1 << j)) {
				if ((*state)->_spill_base) {
					if (__UNW_NO_RESTORE != (*state)->_fr[i]._code) {
						(*state)->_fr[i]._reg = (__uint32_t)(*state)->_spill_order++;
						(*state)->_fr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						(*state)->_fr[i]._offset = (*state)->_spill_base +
										(*state)->_spill_offset;
						(*state)->_spill_offset += sizeof(long double);
					}
				} else {
					(*state)->_fr[i]._reg = (__uint32_t)(*state)->_spill_order++;
					if (__UNW_NO_RESTORE != (*state)->_fr[i]._code) {
						(*state)->_fr[i]._code = __UNW_TO_RESTORE_PSP_RELATIVE;
					}
					(*state)->_fr[i]._offset = (*state)->_spill_offset;
					(*state)->_spill_offset += sizeof(long double);
				}
			}
		}
	} else if (__UNW_IS_P6(*ptr)) {
		count = 1;
		loc = (unsigned char)(*ptr & 0x0f);
		if (0x00 == (*ptr & 0x10)) {
			for (i = __UNW_FR_LOW_START, j = 0; i <= __UNW_FR_LOW_END; i++, j++) {
				if (loc & (0x1 << j)) {
					if ((*state)->_spill_base) {
						(*state)->_fr[i]._reg = (__uint32_t)(*state)->_spill_order++;
						if (__UNW_NO_RESTORE != (*state)->_fr[i]._code) {
							(*state)->_fr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						}
						(*state)->_fr[i]._offset = (*state)->_spill_base +
										(*state)->_spill_offset;
						(*state)->_spill_offset += sizeof(long double);
					} else {
						(*state)->_fr[i]._reg = (__uint32_t)(*state)->_spill_order++;
						if (__UNW_NO_RESTORE != (*state)->_fr[i]._code) {
							(*state)->_fr[i]._code = __UNW_TO_RESTORE_PSP_RELATIVE;
						}
						(*state)->_fr[i]._offset = (*state)->_spill_offset;
						(*state)->_spill_offset += sizeof(long double);
					}
				}
			}
		} else {
			for (i = __UNW_GR_STD_START, j = 0; i <= __UNW_GR_STD_END; i++, j++) {
				if (loc & (0x1 << j)) {
					if ((*state)->_spill_base) {
						(*state)->_gr[i]._reg = (__uint32_t)(*state)->_spill_order++;
						if (__UNW_NO_RESTORE != (*state)->_gr[i]._code) {
							(*state)->_gr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						}
						(*state)->_gr[i]._offset = (*state)->_spill_base +
										(*state)->_spill_offset;
						(*state)->_spill_offset += sizeof(__uint64_t);
					} else {
						(*state)->_gr[i]._reg = (__uint32_t)(*state)->_spill_order++;
						if (__UNW_NO_RESTORE != (*state)->_gr[i]._code) {
							(*state)->_gr[i]._code = __UNW_TO_RESTORE_PSP_RELATIVE;
						}
						(*state)->_gr[i]._offset = (*state)->_spill_offset;
						(*state)->_spill_offset += sizeof(__uint64_t);
					}
				}
			}
		}
	} else if (__UNW_IS_P7(*ptr)) {
		count = 1;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		val1 = val;
		if (0x00 == (*ptr & 0x0f)) {

			/* mem_stack_f only */
			ret = __leb128_decode(ptr+count, size, &val);
			count += ret;
			val2 = val;
		}
		switch (*ptr & 0x0f) {
		    case 0:
			if (slots > total_slots + val1) {
				(*state)->_gr[__UNW_SP]._code = __UNW_RESTORE_FIXED_VALUE;
				(*state)->_frame_size = val2;
			} else {
				(*state)->_gr[__UNW_SP]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 1:
			if (slots <= total_slots + val1) {
				(*state)->_gr[__UNW_SP]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 2:
			if ((*state)->_spill_base) {
				(*state)->_spill_base = val;
				(*state)->_spill_offset = 0L;
			} else {
				(*state)->_spill_base = val;
				for (i = __UNW_GR_STD_START; i <= __UNW_GR_STD_END; i++) {
					if (__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_gr[i]._code) {
						(*state)->_gr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						(*state)->_gr[i]._offset += (*state)->_spill_base;
						val = (*state)->_gr[i]._offset + sizeof(__uint64_t);
					}
				}
				for (i = __UNW_FR_STD_START; i <= __UNW_FR_STD_END; i++) {
					if (__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_fr[i]._code) {
						(*state)->_fr[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						(*state)->_fr[i]._offset += (*state)->_spill_base;
						val = (*state)->_fr[i]._offset + sizeof(long double);
					}
				}
				for (i = __UNW_BR_STD_START; i <= __UNW_BR_STD_END; i++) {
					if (__UNW_TO_RESTORE_PSP_RELATIVE == (*state)->_br[i]._code) {
						(*state)->_br[i]._code = __UNW_RESTORE_PSP_RELATIVE;
						(*state)->_br[i]._offset += (*state)->_spill_base;
						val = (*state)->_br[i]._offset + sizeof(__uint64_t);
					}
				}
				(*state)->_spill_offset = val - (*state)->_spill_base;
			}
			if ((*state)->_spill_base > (*state)->_frame_size) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_process_prologue_desc() ERROR: %s",
						"P7 processing failed with stack frame overflow");
				}
			}
			break;
		    case 3:
			if (__UNW_NO_RESTORE != (*state)->_gr[__UNW_SP]._code) {
				(*state)->_gr[__UNW_SP]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_gr[__UNW_SP]._offset = val1;
			break;
		    case 4:
			if (slots <= total_slots + val1) {
				(*state)->_br[__UNW_RP]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 5:
			if (__UNW_NO_RESTORE != (*state)->_br[__UNW_RP]._code) {
				(*state)->_br[__UNW_RP]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_br[__UNW_RP]._offset = val1;
			break;
		    case 6:
			if (slots <= total_slots + val1) {
				(*state)->_ar[__UNW_PFS]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 7:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_PFS]._code) {
				(*state)->_ar[__UNW_PFS]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_PFS]._offset = val1;
			break;
		    case 8:
			if (slots <= total_slots + val1) {
				(*state)->_preds._code = __UNW_NO_RESTORE;
			}
			break;
		    case 9:
			if (__UNW_NO_RESTORE != (*state)->_preds._code) {
				(*state)->_preds._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_preds._offset = val1;
			break;
		    case 10:
			if (slots <= total_slots + val1) {
				(*state)->_ar[__UNW_LC]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 11:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_LC]._code) {
				(*state)->_ar[__UNW_LC]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_LC]._offset = val1;
			break;
		    case 12:
			if (slots <= total_slots + val1) {
				(*state)->_ar[__UNW_UNAT]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 13:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_UNAT]._code) {
				(*state)->_ar[__UNW_UNAT]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_UNAT]._offset = val1;
			break;
		    case 14:
			if (slots <= total_slots + val1) {
				(*state)->_ar[__UNW_FPSR]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 15:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_FPSR]._code) {
				(*state)->_ar[__UNW_FPSR]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_FPSR]._offset = val1;
			break;
		    default:
			break;
		}
	} else if (__UNW_IS_P8(*ptr)) {
		count = 2;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		switch (*(ptr+1)) {
		    case 0:
			break;
		    case 1:
			if (__UNW_NO_RESTORE != (*state)->_br[__UNW_RP]._code) {
				(*state)->_br[__UNW_RP]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_br[__UNW_RP]._offset = val;
			break;
		    case 2:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_PFS]._code) {
				(*state)->_ar[__UNW_PFS]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_PFS]._offset = val;
			break;
		    case 3:
			if (__UNW_NO_RESTORE != (*state)->_preds._code) {
				(*state)->_preds._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_preds._offset = val;
			break;
		    case 4:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_LC]._code) {
				(*state)->_ar[__UNW_LC]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_LC]._offset = val;
			break;
		    case 5:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_UNAT]._code) {
				(*state)->_ar[__UNW_UNAT]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_UNAT]._offset = val;
			break;
		    case 6:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_FPSR]._code) {
				(*state)->_ar[__UNW_FPSR]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_FPSR]._offset = val;
			break;
		    case 7:
			if (slots <= total_slots + val) {
				(*state)->_ar[__UNW_BSP]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 8:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSP]._code) {
				(*state)->_ar[__UNW_BSP]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_BSP]._offset = val;
			break;
		    case 9:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSP]._code) {
				(*state)->_ar[__UNW_BSP]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_BSP]._offset = val;
			break;
		    case 10:
			if (slots <= total_slots + val) {
				(*state)->_ar[__UNW_BSPSTORE]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 11:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSPSTORE]._code) {
				(*state)->_ar[__UNW_BSPSTORE]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_BSPSTORE]._offset = val;
			break;
		    case 12:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_BSPSTORE]._code) {
				(*state)->_ar[__UNW_BSPSTORE]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_BSPSTORE]._offset = val;
			break;
		    case 13:
			if (slots <= total_slots + val) {
				(*state)->_ar[__UNW_RNAT]._code = __UNW_NO_RESTORE;
			}
			break;
		    case 14:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_RNAT]._code) {
				(*state)->_ar[__UNW_RNAT]._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_ar[__UNW_RNAT]._offset = val;
			break;
		    case 15:
			if (__UNW_NO_RESTORE != (*state)->_ar[__UNW_RNAT]._code) {
				(*state)->_ar[__UNW_RNAT]._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_ar[__UNW_RNAT]._offset = val;
			break;
		    case 16:
			if (slots <= total_slots + val) {
				(*state)->_priunat._code = __UNW_NO_RESTORE;
			}
			break;
		    case 17:
			if (__UNW_NO_RESTORE != (*state)->_priunat._code) {
				(*state)->_priunat._code = __UNW_RESTORE_PSP_RELATIVE;
			}
			(*state)->_priunat._offset = val;
			break;
		    case 18:
			if (__UNW_NO_RESTORE != (*state)->_priunat._code) {
				(*state)->_priunat._code = __UNW_RESTORE_SP_RELATIVE;
			}
			(*state)->_priunat._offset = val;
			break;
		    default:
			break;
		}
	} else if (__UNW_IS_P9(*ptr)) {
		count = 3;
		loc = (unsigned char)(*(ptr+1) & 0x0f);
		reg = (__uint32_t)(*(ptr+2) & 0x7f);
		for (i = __UNW_GR_STD_START, j = 0; i <= __UNW_GR_STD_END; i++, j++) {
			if (loc & (0x1 << j)) {
				if (__UNW_NO_RESTORE != (*state)->_gr[i]._code) {
					(*state)->_gr[i]._code = __UNW_RESTORE_OFF_GR;
				}
				(*state)->_gr[i]._reg = reg++;
			}
		}
	} else if (__UNW_IS_P10(*ptr)) {
		count = 3;
		/* grmask = (unsigned char)(*(ptr+1) & 0x0f); */
		/* gr = (unsigned char)(*(ptr+2) & 0x7f); */
	} else if (__UNW_IS_X1(*ptr)) {
		count = 2;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
	} else if (__UNW_IS_X2(*ptr)) {
		count = 3;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_prologue_desc() ERROR: %s 0x%x",
				"invalid prologue descriptor", *ptr & 0xff);
		}
	}
	return count;
}



/* body unwind descriptor processing */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_process_body_desc(__unw_state_info_t **state,
						__uint64_t slots, __uint64_t total_slots,
						__uint64_t rlen, char *ptr, __uint64_t size) {
	__unw_error_t ret;
	__uint64_t count = 0L, num, val, val1, val2;

	if (__UNW_IS_B1(*ptr)) {
		count = 1;
		val = (__uint64_t)(*ptr & 0x1f);
		/* update state and labels */
		if (0x00 == (*ptr & 0x20)) {
			(*state)->_label = val;
		} else {
			if (slots < total_slots + rlen) {
				if (__UNW_OK != (ret = unwind_state_stack_search(state, val))) {
					return ret;
				}
			}
		}
	} else if (__UNW_IS_B2(*ptr)) {
		count = 1;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		/* update state */
		if (slots < total_slots + rlen) {
			if (slots > total_slots + rlen - val) {
				(*state)->_gr[__UNW_SP]._code = __UNW_NO_RESTORE;
			}
		} else {
			if (__UNW_OK != (ret = unwind_state_stack_pop(
						(__uint64_t)((*ptr & 0x1f) + 1)))) {
				return ret;
			}
			if (__UNW_OK != (ret = unwind_state_stack_top(state))) {
				return ret;
			}
		}
	} else if (__UNW_IS_B3(*ptr)) {
		count = 1;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		val1 = val;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		val2 = val;
		/* update state */
		if (slots < total_slots + rlen) {
			if (slots > total_slots + rlen - val) {
				(*state)->_gr[__UNW_SP]._code = __UNW_NO_RESTORE;
			}
		} else {
			if (__UNW_OK != (ret = unwind_state_stack_pop(val2 + 1))) {
				return ret;
			}
			if (__UNW_OK != (ret = unwind_state_stack_top(state))) {
				return ret;
			}
		}
	} else if (__UNW_IS_B4(*ptr)) {
		count = 1;
		num = __leb128_decode(ptr+count, size, &val);
		count += num;
		/* update state and labels */
		if (0x00 == (*ptr & 0x08)) {
			(*state)->_label = val;
		} else {
			if (slots < total_slots + rlen) {
				if (__UNW_OK != (ret = unwind_state_stack_search(state, val))) {
					return ret;
				}
			}
		}
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_body_desc() ERROR: %s",
				"invalid body descriptor");
		}
	}
	return count;
}



/* consumer function to process the descriptors */
__unw_error_t unwind_process_desc(__uint64_t slots, __unw_info_t *info,
						__unw_state_info_t *state) {
	__unw_state_info_t *state_ptr;
	__uint32_t ip_found = 0;
	__uint64_t rlen, count = 0L, size = 0L, retnum = 0L, total_slots = 0L;
	char *ptr = NULL;
	enum { __UNW_UNDEF, __UNW_PROLOGUE, __UNW_BODY } desc_id = __UNW_UNDEF;

	/* calculate unwind info size */
	size = (__uint64_t)(__UNW_LENGTH((info->_header) *
					sizeof(__unw_dbl_word_t)));

	/* check version */
	if (__UNW_VER(info->_header) != 1) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_desc() ERROR: %s (%d)",
				"invalid version", __UNW_VER(info->_header));
		}
		return __UNW_INV_VERSION_ERROR;
	}

	/* reset unwind state stack */
	if (__UNW_OK != unwind_state_stack_reset()) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_desc() ERROR: %s",
				"can not reset state stack");
			return __UNW_INTERNAL_ERROR;
		}
	}

	/* process unwind descriptors */
	ptr = (char *)info->_body;
	count = 0L;
	while (count < size) {
		if (__UNW_IS_HEADER(*ptr)) {
			/* if IP has been found, then no need to keep on */
			if (ip_found) {
				break;
			}
			if (__UNW_IS_PROLOGUE_HEADER(*ptr)) {
				desc_id = __UNW_PROLOGUE;
				if (0 == (retnum = unwind_process_header_desc(&state_ptr,
						slots, total_slots, &rlen, ptr, size-count))) {
					return __UNW_INTERNAL_ERROR;
				}
				total_slots += rlen;
				if (slots < total_slots) {
					/* IP found */
					ip_found = 1;
				}
			} else if (__UNW_IS_BODY_HEADER(*ptr)) {
				desc_id = __UNW_BODY;
				if (0 == (retnum = unwind_process_header_desc(&state_ptr,
						slots, total_slots, &rlen, ptr, size-count))) {
					return __UNW_INTERNAL_ERROR;
				}
				total_slots += rlen;
				if (slots < total_slots) {
					/* IP found */
					ip_found = 1;
				}
			} else {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_process_desc() ERROR: %s (0x%x)",
					"invalid header",
					(unsigned char)*ptr);
				}
				return __UNW_INTERNAL_ERROR;
			}
		} else {
			if (__UNW_PROLOGUE == desc_id) {
			        fprintf(stderr, "\t\t\t\tcount = 0x%llx\n", count);
				if (0 == (retnum = unwind_process_prologue_desc(&state_ptr,
						slots, total_slots - rlen, rlen, ptr, size-count))) {
					return __UNW_INTERNAL_ERROR;
				}
			} else if (__UNW_BODY == desc_id) {
				if (0 == (retnum = unwind_process_body_desc(&state_ptr,
						slots, total_slots - rlen, rlen, ptr, size-count))) {
					return __UNW_INTERNAL_ERROR;
				}
			} else {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_process_desc() ERROR: %s",
					"invalid prologue/body");
				}
				return __UNW_INTERNAL_ERROR;
			}
		}
		count += retnum;
		ptr += retnum;
	}

	/* check if IP has been found */
	if (!ip_found) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_process_desc() ERROR: %s",
				"ip not found in unwind info");
		}
		return __UNW_INTERNAL_ERROR;
	}

	/* set state */
	*state = *state_ptr;

	return __UNW_OK;
}

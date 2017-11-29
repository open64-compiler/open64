/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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
#include <elf.h>
#include <sys/unwind.h>



/* header unwind descriptor dumping */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_dump_header_desc(FILE *fp, __uint64_t *rlen,
						char *ptr, __uint64_t size) {
	__uint64_t count = 0L, ret, val;
	unsigned char loc;

	if (__UNW_IS_R1(*ptr)) {
		count = 1;
		fprintf(fp, "\tR1: 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned long long)count);
		*rlen = (__uint64_t)((*ptr) & 0x1f);
		if (0x00 == (*ptr & 0x20)) {
			fprintf(fp, "\t\tPrologue header, size %llu\n",
				(unsigned long long)*rlen);
		} else {
			fprintf(fp, "\t\tBody header, size %llu\n",
				(unsigned long long)*rlen);
		}
	} else if (__UNW_IS_R2(*ptr)) {
		count = 2;
		fprintf(fp, "\tR2: 0x%02x 0x%02x ",
			(unsigned char)*ptr, (unsigned char)*(ptr+1));
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		*rlen = val;
		fprintf(fp, "rlen=%llu (desc_size=%llu)\n",
			(unsigned long long)*rlen,
			(unsigned long long)count);
		loc = (unsigned char)(*ptr & 0x07);
		loc <<= 1;
		loc |= ((*(ptr+1) & 0x80) >> 7);
		fprintf(fp, "\t\tPrologue header, size %llu, ",
			(unsigned long long)*rlen);
		fprintf(fp, "mask (rp, ar.pfs, psp, predicates) %01x, grsave %u\n",
			loc, (unsigned char)(*(ptr+1) & 0x7f));
	} else if (__UNW_IS_R3(*ptr)) {
		count = 1;
		fprintf(fp, "\tR3: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		*rlen = val;
		fprintf(fp, "rlen=%llu (desc_size=%llu)\n",
			(unsigned long long)*rlen,
			(unsigned long long)count);
		if (0x00 == (*ptr & 0x03)) {
			fprintf(fp, "\t\tPrologue header, size %llu\n",
				(unsigned long long)*rlen);
		} else {
			fprintf(fp, "\t\tBody header, size %llu\n",
				(unsigned long long)*rlen);
		}
	} else {
		fprintf(fp, "\tInvalid header descriptor\n");
	}
	return count;
}



/* prologue unwind descriptor dumping */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_dump_prologue_desc(FILE *fp, __uint64_t rlen,
						char *ptr, __uint64_t size) {
	__uint64_t count = 0L, ret, val, i;
	unsigned char loc;

	if (__UNW_IS_P1(*ptr)) {
		count = 1;
		fprintf(fp, "\tP1: 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned long long)count);
		fprintf(fp, "\t\tType br_mem, brmask 0x%02x\n",
			(unsigned char)(*ptr & 0x1f));
	} else if (__UNW_IS_P2(*ptr)) {
		count = 2;
		fprintf(fp, "\tP2: 0x%02x 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr, (unsigned char)*(ptr+1),
			(unsigned long long)count);
		loc = (unsigned char)(*ptr & 0x0f);
		loc <<= 1;
		loc |= ((*(ptr+1) & 0x80) >> 7);
		fprintf(fp, "\t\tType br_gr, brmask 0x%02x, gr %u\n",
			loc, (unsigned char)(*(ptr+1) & 0x7f));
	} else if (__UNW_IS_P3(*ptr)) {
		char *type = "unknown";
		char *reg = "gr/br";

		count = 2;
		fprintf(fp, "\tP3: 0x%02x 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr, (unsigned char)*(ptr+1),
			(unsigned long long)count);
		loc = (unsigned char)(*ptr & 0x07);
		loc <<= 1;
		loc |= ((*(ptr+1) & 0x80) >> 7);
		switch (loc) {
		    case 0:
			type = "psp_gr";
			reg = "gr";
			break;
		    case 1:
			type = "rp_gr";
			reg = "gr";
			break;
		    case 2:
			type = "pfs_gr";
			reg = "gr";
			break;
		    case 3:
			type = "pred_gr";
			reg = "gr";
			break;
		    case 4:
			type = "unat_gr";
			reg = "gr";
			break;
		    case 5:
			type = "lc_gr";
			reg = "gr";
			break;
		    case 6:
			type = "rp_br";
			reg = "br";
			break;
		    case 7:
			type = "rnat_gr";
			reg = "gr";
			break;
		    case 8:
			type = "bsp_gr";
			reg = "gr";
			break;
		    case 9:
			type = "bspstore_gr";
			reg = "gr";
			break;
		    case 10:
			type = "fpsr_gr";
			reg = "gr";
			break;
		    case 11:
			type = "priunat_gr";
			reg = "gr";
			break;
		    default:
			break;
		}
		fprintf(fp, "\t\tType %s, %s %u\n", type, reg,
			(unsigned char)(*(ptr+1) & 0x7f));
	} else if (__UNW_IS_P4(*ptr)) {
		__uint64_t desc_size = ((rlen * 2) + 7 ) / 8,
			byte_no = 0L, shift_no = 0L;

		count = 1;
		fprintf(fp, "\tP4: 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned long long)(count + desc_size));
		if (1 + desc_size > size) {
			fprintf(fp, "\n\t\tInvalid size for descriptor P4 (spill_mask)\n");
			count += desc_size;
		} else {
			fprintf(fp, "\t\tType spill_mask, imask size in doublebits = %llu\n",
				(unsigned long long)rlen);
			fprintf(fp, "\t\tDumping imask: ");
			for (i = 0; i < (rlen * 2); i += 2) {
				byte_no = (i / 8) + 1;
				shift_no = i % 8;

				loc = (unsigned char)(*(ptr + byte_no));
				loc <<= shift_no;
				loc >>= 6;
				fprintf(fp, "%01x", loc);
			}
			fprintf(fp, "\n");
			count += desc_size;
		}
	} else if (__UNW_IS_P5(*ptr)) {
		__uint32_t frmask = 0;

		count = 4;
		fprintf(fp, "\tP5: 0x%02x 0x%02x 0x%02x 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr, (unsigned char)*(ptr+1),
			(unsigned char)*(ptr+2), (unsigned char)*(ptr+3),
			(unsigned long long)count);
		frmask = (__uint32_t)((*(ptr+1) & 0x0f));
		frmask <<= 4;
		frmask |= *(ptr+2);
		frmask <<= 8;
		frmask |= *(ptr+3);
		fprintf(fp, "\t\tType frgr_mem, grmask 0x%01x, frmask %05x\n",
			(unsigned char)((*(ptr+1) & 0xf0) >> 4), frmask);
	} else if (__UNW_IS_P6(*ptr)) {
		count = 1;
		fprintf(fp, "\tP6: 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned long long)count);
		if (0x00 == (*ptr & 0x10)) {
			fprintf(fp, "\tType fr_mem, frmask %01x\n",
				(unsigned char)(*ptr & 0x0f));
		} else {
			fprintf(fp, "\tType gr_mem, grmask %01x\n",
				(unsigned char)(*ptr & 0x0f));
		}
	} else if (__UNW_IS_P7(*ptr)) {
		char *type = "unknown";

		count = 1;
		fprintf(fp, "\tP7: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "t/spoff/pspoff=%llu ",
			(unsigned long long)val);
		if (0x00 == (*ptr & 0x0f)) {
			/* mem_stack_f only */
			ret = __leb128_decode(ptr+count, size, &val);
			count += ret;
			fprintf(fp, "size=%llu*16 (desc_size=%llu)\n",
				(unsigned long long)val,
				(unsigned long long)count);
		} else {
			fprintf(fp, "(desc_size=%llu)\n", (unsigned long long)count);
		}
		switch (*ptr & 0x0f) {
		    case 0:
			type = "mem_stack_f";
			break;
		    case 1:
			type = "mem_stack_v";
			break;
		    case 2:
			type = "spill_base";
			break;
		    case 3:
			type = "psp_sprel";
			break;
		    case 4:
			type = "rp_when";
			break;
		    case 5:
			type = "rp_psprel";
			break;
		    case 6:
			type = "pfs_when";
			break;
		    case 7:
			type = "pfs_psprel";
			break;
		    case 8:
			type = "preds_when";
			break;
		    case 9:
			type = "preds_psprel";
			break;
		    case 10:
			type = "lc_when";
			break;
		    case 11:
			type = "lc_psprel";
			break;
		    case 12:
			type = "unat_when";
			break;
		    case 13:
			type = "unat_psprel";
			break;
		    case 14:
			type = "fpsr_when";
			break;
		    case 15:
			type = "fpsr_psprel";
			break;
		    default:
			break;
		}
		fprintf(fp, "\t\tType %s\n", type);
	} else if (__UNW_IS_P8(*ptr)) {
		char *type = "unknown";

		count = 2;
		fprintf(fp, "\tP8: 0x%02x 0x%02x ",
			(unsigned char)*ptr, (unsigned char)*(ptr+1));
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "t/spoff/pspoff=%llu (desc_size=%llu)\n",
			(unsigned long long)val,
			(unsigned long long)count);
		switch (*(ptr+1)) {
		    case 0:
			type = "unused";
			break;
		    case 1:
			type = "rp_sprel";
			break;
		    case 2:
			type = "pfs_sprel";
			break;
		    case 3:
			type = "preds_sprel";
			break;
		    case 4:
			type = "lc_sprel";
			break;
		    case 5:
			type = "unat_sprel";
			break;
		    case 6:
			type = "fpsr_sprel";
			break;
		    case 7:
			type = "bsp_when";
			break;
		    case 8:
			type = "bsp_psprel";
			break;
		    case 9:
			type = "bsp_sprel";
			break;
		    case 10:
			type = "bspstore_when";
			break;
		    case 11:
			type = "bspstore_psprel";
			break;
		    case 12:
			type = "bspstore_sprel";
			break;
		    case 13:
			type = "rnat_when";
			break;
		    case 14:
			type = "rnat_psprel";
			break;
		    case 15:
			type = "rnat_sprel";
			break;
		    case 16:
			type = "priunat_when";
			break;
		    case 17:
			type = "priunat_psprel";
			break;
		    case 18:
			type = "priunat_sprel";
			break;
		    default:
			break;
		}
		fprintf(fp, "\t\tType %s\n", type);
	} else if (__UNW_IS_P9(*ptr)) {
		count = 3;
		fprintf(fp, "\tP9: 0x%02x 0x%02x 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned char)*(ptr+1),
			(unsigned char)*(ptr+2),
			(unsigned long long)count);
		fprintf(fp, "\t\tType gr_gr, grmask 0x%01x, gr %u\n",
			(unsigned char)(*(ptr+1) & 0x0f),
			(unsigned char)(*(ptr+2) & 0x7f));
	} else if (__UNW_IS_P10(*ptr)) {
		count = 3;
		fprintf(fp, "\tP10: 0x%02x 0x%02x 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned char)*(ptr+1),
			(unsigned char)*(ptr+2),
			(unsigned long long)count);
		fprintf(fp, "\t\tType ABI-specific, abi %u, context 0x%02x\n",
			(unsigned char)*(ptr+1),
			(unsigned char)*(ptr+2));
	} else if (__UNW_IS_X1(*ptr)) {
		count = 2;
		fprintf(fp, "\tX1: 0x%02x", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "time=%llu\n", (unsigned long long) val);
		fprintf(fp, "\t\treg = 0x%0x, ",
			(unsigned char)*(ptr+1));
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "offset = %lld\n", (long long)val);
	} else if (__UNW_IS_X2(*ptr)) {
		count = 3;
		fprintf(fp, "\tX1: 0x%02x", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "time=%llu\n",
			(unsigned long long) val);
		fprintf(fp, "\t\treg = 0x%0x, ",
			(unsigned char)*(ptr+1));
		fprintf(fp, "treg = 0x%0x\n",
			(unsigned char)*(ptr+2));
	} else {
		fprintf(fp, "\tInvalid prologue descriptor\n");
	}
	return count;
}



/* body unwind descriptor dumping */
/* (returns descriptor size in bytes) */
static __uint64_t unwind_dump_body_desc(FILE *fp, __uint64_t rlen,
						char *ptr, __uint64_t size) {
	__uint64_t count = 0L, ret, val, val1, val2;

	if (__UNW_IS_B1(*ptr)) {
		count = 1;
		fprintf(fp, "\tB1: 0x%02x (desc_size=%llu)\n",
			(unsigned char)*ptr,
			(unsigned long long)count);
		if (0x00 == (*ptr & 0x20)) {
			fprintf(fp, "\t\tBody label state %u\n",
				(unsigned char)(*ptr & 0x1f));
		} else {
			fprintf(fp, "\t\tBody copy state %u\n",
				(unsigned char)(*ptr & 0x1f));
		}
	} else if (__UNW_IS_B2(*ptr)) {
		count = 1;
		fprintf(fp, "\tB2: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "t=%llu (desc_size=%llu)\n",
			(unsigned long long)val,
			(unsigned long long)count);
		fprintf(fp, "\t\tEpilogue, %u nested prologues, ",
			(unsigned char)(*ptr & 0x1f));
		fprintf(fp, "time -%llu (body size %llu)\n",
			(unsigned long long)val,
			(unsigned long long)rlen);
	} else if (__UNW_IS_B3(*ptr)) {
		count = 1;
		fprintf(fp, "\tB3: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "t=%llu ", (unsigned long long)val);
		val1 = val;
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "ecount=%llu ", (unsigned long long)val);
		val2 = val;
		fprintf(fp, "(desc_size=%llu)\n", (unsigned long long)count);
		fprintf(fp, "\t\tEpilogue, %llu nested prologues, ",
			(unsigned long long)val2);
		fprintf(fp, "time -%llu (body size %llu)\n",
			(unsigned long long)val1,
			(unsigned long long)rlen);
	} else if (__UNW_IS_B4(*ptr)) {
		count = 1;
		fprintf(fp, "\tB4: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "label=%llu (desc_size=%llu)\n",
			(unsigned long long)val,
			(unsigned long long)count);
		if (0x00 == (*ptr & 0x08)) {
			fprintf(fp, "\t\tBody label state %llu\n",
				(unsigned long long)val);
		} else {
			fprintf(fp, "\t\tBody copy state %llu\n",
				(unsigned long long)val);
		}
	} else if (__UNW_IS_X1(*ptr)) {
		count = 2;
		fprintf(fp, "\tX1: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "time=%llu\n", (unsigned long long) val);
		fprintf(fp, "\t\treg = 0x%0x, ",
			(unsigned char)*(ptr+1));
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "offset = %lld\n", (long long)val);
	} else if (__UNW_IS_X2(*ptr)) {
		count = 3;
		fprintf(fp, "\tX2: 0x%02x ", (unsigned char)*ptr);
		ret = __leb128_decode(ptr+count, size, &val);
		count += ret;
		fprintf(fp, "time=%llu\n",
			(unsigned long long) val);
		fprintf(fp, "\t\treg = 0x%0x, ",
			(unsigned char)*(ptr+1));
		fprintf(fp, "treg = 0x%0x\n",
			(unsigned char)*(ptr+2));
	} else {
		fprintf(fp, "\tInvalid body descriptor\n");
	}
	return count;
}



/* unwind table and/or unwind info dumping to ascii */
/* convention: last arg is file pointer */
__unw_error_t unwind_dump2ascii(char *unwind_table_ptr,
					__uint64_t unwind_table_size,
					char *unwind_info_ptr,
					__uint64_t unwind_info_size,
					void *arg) {
	FILE *fp = (NULL != arg) ? (FILE *)arg : stdout;
	__uint64_t i, size = 0L, count = 0L, retnum = 0L, unwind_info_offset = 0L,
		rlen = 0L, unwind_table_size_in_entries =
				unwind_table_size / sizeof(__unw_table_entry_t);
	__unw_table_entry_t *unwind_table =
		(__unw_table_entry_t *)unwind_table_ptr;
	__unw_info_t *info = NULL;
	char *ptr = NULL;
	enum { __UNW_UNDEF, __UNW_PROLOGUE, __UNW_BODY } desc_id = __UNW_UNDEF;
	__unw_error_t ret = __UNW_OK;

	/* dump unwind table */
	if (NULL != unwind_table_ptr) {
		fprintf(fp, "\n\t\t*** UNWIND TABLE ***\n");
		for (i = 0; i < unwind_table_size_in_entries; i++) {
			fprintf(fp, "Unwind table entry: %03llu   ",
				(unsigned long long)i);
			fprintf(fp, "Addresses: 0x%08llx - 0x%08llx   ",
				(unsigned long long)unwind_table[i]._start,
				(unsigned long long)unwind_table[i]._end);
			fprintf(fp, "Unwind info: 0x%08llx\n",
				(unsigned long long)unwind_table[i]._info);
		}
		fprintf(fp, "\n");
	}

	/* dump unwind info */
	if (NULL != unwind_info_ptr) {
		fprintf(fp, "\n\t\t*** UNWIND INFO ***\n");
		while (unwind_info_offset < unwind_info_size) {
			info = (__unw_info_t *)((char *)unwind_info_ptr +
								unwind_info_offset);
			size = (__uint64_t)(__UNW_LENGTH((info->_header) *
							sizeof(__unw_dbl_word_t)));

			/* dump unwind info */
			fprintf(fp, "Unwind info @ %p ", info);
			fprintf(fp, "(Offset: 0x%llx)\n",
				(unsigned long long)unwind_info_offset);
			fprintf(fp, "\tVersion: %llu   ",
				(unsigned long long)__UNW_VER(info->_header));
			fprintf(fp, "Flags EHANDLER/UHANDLER: %llu/%llu   ",
				(unsigned long long)__UNW_FLAG_EHANDLER(info->_header),
				(unsigned long long)__UNW_FLAG_UHANDLER(info->_header));
			fprintf(fp, "Size: 1+%llu\n", (unsigned long long)(size /
								sizeof(__unw_dbl_word_t)));

			/* dump personality routine address */
			if (__UNW_FLAG_EHANDLER(info->_header) ||
						__UNW_FLAG_UHANDLER(info->_header)) {
				fprintf(fp, "Personality routine: %llx\n",
					(unsigned long long)(*(__unw_addr_t *)
					((char *)unwind_info_ptr + unwind_info_offset +
					sizeof(__unw_dbl_word_t) + size)));
			}

			/* check sizes */
			if (unwind_info_offset + sizeof(__unw_dbl_word_t) + size >
									unwind_info_size) {
				fprintf(fp, "\tInvalid size\n");
				ret = __UNW_INV_SIZE_ERROR;
				break;
			}

			/* check version */
			if (__UNW_VER(info->_header) != 1) {
				fprintf(fp, "\tInvalid version\n");
				ret = __UNW_INV_VERSION_ERROR;
				break;
			}

			/* dump unwind descriptors */
			ptr = (char *)info->_body;
			count = 0L;
			while (count < size) {
				if (__UNW_IS_HEADER(*ptr)) {
					if (__UNW_IS_PROLOGUE_HEADER(*ptr)) {
						desc_id = __UNW_PROLOGUE;
						retnum = unwind_dump_header_desc(fp,
								&rlen, ptr, size-count);
					} else if (__UNW_IS_BODY_HEADER(*ptr)) {
						desc_id = __UNW_BODY;
						retnum = unwind_dump_header_desc(fp,
								&rlen, ptr, size-count);
					} else {
						fprintf(fp, "\tInvalid header (0x%x)\n",
							(unsigned char)*ptr);
						break;
					}
				} else {
					if (__UNW_PROLOGUE == desc_id) {
						retnum = unwind_dump_prologue_desc(fp,
								rlen, ptr, size-count);
					} else if (__UNW_BODY == desc_id) {
						retnum = unwind_dump_body_desc(fp,
								rlen, ptr, size-count);
					} else {
						fprintf(fp, "\tInvalid prologue/body\n");
						break;
					}
				}
				if (0 == retnum) {
					break;
				}
				count += retnum;
				ptr += retnum;
			}

			/* move on */
			unwind_info_offset += (sizeof(__unw_dbl_word_t) + size);
			if (__UNW_FLAG_EHANDLER(info->_header) ||
						__UNW_FLAG_UHANDLER(info->_header)) {
				__uint64_t real_size = (sizeof(__unw_addr_t) + 
						sizeof(__unw_dbl_word_t) - 1) /
						sizeof(__unw_dbl_word_t);
				real_size *= sizeof(__unw_dbl_word_t);
				unwind_info_offset += real_size;
			}
		}
		fprintf(fp, "\n");
	}

	return ret;
}

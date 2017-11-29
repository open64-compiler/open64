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
#include <strings.h>
#include <sys/unwindP.h>
#include "unwind_producer.h"



/* producer function to add a prologue header descriptor */
__unw_error_t unwind_info_add_prologue_header(__unw_info_t *info, __uint64_t size) {
	__unw_error_t ret = __UNW_OK;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* don't forget to add the imask if this is a prologue and there's an imask */
	/* (imask gets set in overall.c) */
	if ((__UNW_PROLOGUE == _current_region_id) && (0L != _imask_size)) {
		unwind_info_add_imask(info);
	}

	/* check sizes */
	_current_procedure_size += size;
	_current_region_total_size = size;
	_current_region_id = __UNW_PROLOGUE;
	if (_current_procedure_size > _current_procedure_total_size) {
		return __UNW_INV_SIZE_ERROR;
	}

	/* construct and add the prologue header descriptor */
	if (size < 32) {
		/* format R1 */
		/* 7 6 5 4 3 2 1 0 */
		/* 0 0 0 rlen      */
		__unw_format_r1_t desc;
		__uint64_t rsize;

		desc._fix[0] = 0x00;
		desc._fix[0] |= (char)size;
		rsize = 1;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)&desc);
	} else {
		/* format R3 */
		/* 7 6 5 4 3 2 1 0              */
		/* 0 1 1 0 0 0 0 0 rlen(LEB128) */
		__unw_format_r3_t *desc;
		__uint64_t rsize, esize;
		char encoded[1+__UNW_ENCODING_SIZE];

		encoded[0] = 0x60;
		if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				size)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_r3_t *)&encoded;
		rsize = 1 + esize;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	}

	return ret;
}



/* producer function to add a prologue_gr header descriptor */
__unw_error_t unwind_info_add_prologue_gr_header(__unw_info_t *info, __uint64_t size,
							char mask, __uint32_t gr) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_r2_t *desc;
	__uint64_t rsize, esize;
	char encoded[2+__UNW_ENCODING_SIZE];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* don't forget to add the imask if this is a prologue and there's an imask */
	/* (imask gets set in overall.c) */
	if ((__UNW_PROLOGUE == _current_region_id) && (0L != _imask_size)) {
		unwind_info_add_imask(info);
	}

	/* check sizes */
	_current_procedure_size += size;
	_current_region_total_size = size;
	_current_region_id = __UNW_PROLOGUE;
	if (_current_procedure_size > _current_procedure_total_size) {
		return __UNW_INV_SIZE_ERROR;
	}

	/* construct and add the prologue_gr header descriptor */
	/* format R2 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0              */
	/* 0 1 0 0 0 mask    grsave        rlen(LEB128) */
	/*                                              */
	/* where mask bits (from MSB to LSB) correspond to rp, ar.pfs, psp, predicates */
	if ((mask >= 16) || (gr >= 128)) {
		return __UNW_INV_ARG_ERROR;
	}
	encoded[0] = 0x40;
	encoded[0] |= (mask >> 1);
	encoded[1] = (mask << 7);
	encoded[1] |= gr;
	if ((esize = __leb128_encode((char *)&encoded[2],
			(__uint64_t)__UNW_ENCODING_SIZE,
			size)) == 0) {
		return __UNW_INTERNAL_ERROR;
	}

	desc = (__unw_format_r2_t *)&encoded;
	rsize = 2 + esize;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)desc);

	return ret;
}



/* producer function to add a body header descriptor */
__unw_error_t unwind_info_add_body_header(__unw_info_t *info, __uint64_t size) {
	__unw_error_t ret = __UNW_OK;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* don't forget to add the imask if this is a prologue and there's an imask */
	/* (imask gets set in overall.c) */
	if ((__UNW_PROLOGUE == _current_region_id) && (0L != _imask_size)) {
		unwind_info_add_imask(info);
	}

	/* check sizes */
	_current_procedure_size += size;
	_current_region_total_size = size;
	_current_region_id = __UNW_BODY;
	if (_current_procedure_size > _current_procedure_total_size) {
		return __UNW_INV_SIZE_ERROR;
	}

	/* construct and add the body header descriptor */
	if (size < 32) {
		/* format R1 */
		/* 7 6 5 4 3 2 1 0 */
		/* 0 0 1 rlen      */
		__unw_format_r1_t desc;
		__uint64_t rsize;

		desc._fix[0] = 0x20;
		desc._fix[0] |= (char)size;
		rsize = 1;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)&desc);
	} else {
		/* format R3 */
		/* 7 6 5 4 3 2 1 0              */
		/* 0 1 1 0 0 0 0 1 rlen(LEB128) */
		__unw_format_r3_t *desc;
		__uint64_t rsize, esize;
		char encoded[1+__UNW_ENCODING_SIZE];

		encoded[0] = 0x61;
		if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				size)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_r3_t *)&encoded;
		rsize = 1 + esize;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	}

	return ret;
}

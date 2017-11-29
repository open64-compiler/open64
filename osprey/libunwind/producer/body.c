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



/* producer function to add a body epilogue descriptor */
__unw_error_t unwind_info_add_body_epilogue_info(__unw_info_t *info,
						__uint64_t when, __uint64_t ecount) {
	__unw_error_t ret = __UNW_OK;
	__uint64_t rsize, esize, esize1, esize2;
	char encoded[1+2*__UNW_ENCODING_SIZE];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* check valid time argument */
	if (when >= _current_region_total_size) {
		return __UNW_INV_SIZE_ERROR;
	}

	/* construct and add the epilogue descriptor */
	if (ecount < 32) {
		/* format B2 */
		/* 7 6 5 4 3 2 1 0           */ 
		/* 1 1 0 ecount    t(LEB128) */
		__unw_format_b2_t *desc;

		encoded[0] = 0xc0;
		encoded[0] |= (char)ecount;
		if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				when)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_b2_t *)&encoded;
		rsize = 1 + esize;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	} else {
		/* format B3 */
		/* 7 6 5 4 3 2 1 0                          */ 
		/* 1 1 1 0 0 0 0 0 t(LEB128) ecount(LEB128) */
		__unw_format_b3_t *desc;

		encoded[0] = 0xe0;
		if ((esize1 = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				when)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}
		if ((esize2 = __leb128_encode((char *)&encoded[1+esize1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				ecount)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_b3_t *)&encoded;
		rsize = 1 + esize1 + esize2;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	}

	return ret;
}



/* producer function to add a label_state epilogue descriptor */
__unw_error_t unwind_info_add_body_label_state_info(__unw_info_t *info,
						__uint64_t label) {
	__unw_error_t ret = __UNW_OK;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the label_state descriptor */
	if (label < 32) {
		/* format B1 */
		/* 7 6 5 4 3 2 1 0 */ 
		/* 1 0 0 label     */
		__unw_format_b1_t desc;
		__uint64_t rsize;

		desc._fix[0] = 0x80;
		desc._fix[0] |= (char)label;
		rsize = 1;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)&desc);
	} else {
		/* format B4 */
		/* 7 6 5 4 3 2 1 0               */ 
		/* 1 1 1 1 0 0 0 0 label(LEB128) */
		__unw_format_b4_t *desc;
		__uint64_t rsize, esize;
		char encoded[1+__UNW_ENCODING_SIZE];

		encoded[0] = 0xf0;
		if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				label)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_b4_t *)&encoded;
		rsize = 1 + esize;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	}

	return ret;
}



/* producer function to add a copy_state epilogue descriptor */
__unw_error_t unwind_info_add_body_copy_state_info(__unw_info_t *info,
						__uint64_t label) {
	__unw_error_t ret = __UNW_OK;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the copy_state descriptor */
	if (label < 32) {
		/* format B1 */
		/* 7 6 5 4 3 2 1 0 */ 
		/* 1 0 1 label     */
		__unw_format_b1_t desc;
		__uint64_t rsize;

		desc._fix[0] = 0xa0;
		desc._fix[0] |= (char)label;
		rsize = 1;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)&desc);
	} else {
		/* format B4 */
		/* 7 6 5 4 3 2 1 0               */ 
		/* 1 1 1 1 1 0 0 0 label(LEB128) */
		__unw_format_b4_t *desc;
		__uint64_t rsize, esize;
		char encoded[1+__UNW_ENCODING_SIZE];

		encoded[0] = 0xf8;
		if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				label)) == 0) {
			return __UNW_INTERNAL_ERROR;
		}

		desc = (__unw_format_b4_t *)&encoded;
		rsize = 1 + esize;

		/* add descriptor */
		ret = unwind_info_add_desc(rsize, (char *)desc);
	}

	return ret;
}

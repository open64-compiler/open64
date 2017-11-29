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



/* producer function to add a prologue fr_mem descriptor */
__unw_error_t unwind_info_add_prologue_fr_mem_info(__unw_info_t *info,
								__uint32_t frmask) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p6_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the fr_mem descriptor */
	/* format P6 */
	/* 7 6 5 4 3 2 1 0 */ 
	/* 1 1 0 0 frmask  */
	if (frmask >= 16) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0xc0;
	desc._fix[0] |= frmask;
	rsize = 1;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue frgr_mem descriptor */
__unw_error_t unwind_info_add_prologue_frgr_mem_info(__unw_info_t *info,
					__uint32_t grmask, __uint32_t frmask) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p5_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the frgr_mem descriptor */
	/* format P5 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */ 
	/* 1 0 1 1 1 0 0 0 grmask  frmask                                */
	if ((grmask >= 16) || (frmask >= 1048576)) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0xb9;
	desc._fix[1] = (grmask << 4);
	desc._fix[1] |= ((frmask & 0xf0000) >> 16);
	desc._fix[2] = ((frmask & 0x0ff00) >> 8);
	desc._fix[3] = (frmask & 0x000ff);
	rsize = 4;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue gr_gr descriptor */
__unw_error_t unwind_info_add_prologue_gr_gr_info(__unw_info_t *info, __uint32_t grmask,
								__uint32_t gr) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p9_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the gr_gr descriptor */
	/* format P9 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */ 
	/* 1 1 1 1 0 0 0 1 0 0 0 0 grmask  0 gr            */
	if ((grmask >= 16) || (gr >= 128)) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0xf1;
	desc._fix[1] = 0x00;
	desc._fix[1] |= grmask;
	desc._fix[2] = 0x00;
	desc._fix[2] |= gr;
	rsize = 3;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue gr_mem descriptor */
__unw_error_t unwind_info_add_prologue_gr_mem_info(__unw_info_t *info,
								__uint32_t grmask) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p6_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the gr_mem descriptor */
	/* format P6 */
	/* 7 6 5 4 3 2 1 0 */ 
	/* 1 1 0 1 grmask  */
	if (grmask >= 16) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0xd0;
	desc._fix[0] |= grmask;
	rsize = 1;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue br_mem descriptor */
__unw_error_t unwind_info_add_prologue_br_mem_info(__unw_info_t *info,
								__uint32_t brmask) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p1_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the br_mem descriptor */
	/* format P1 */
	/* 7 6 5 4 3 2 1 0 */ 
	/* 1 0 0 brmask    */
	if (brmask >= 32) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0x80;
	desc._fix[0] |= brmask;
	rsize = 1;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue br_gr descriptor */
__unw_error_t unwind_info_add_prologue_br_gr_info(__unw_info_t *info,
						__uint32_t brmask, __uint32_t gr) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p2_t desc;
	__uint64_t rsize;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the br_gr descriptor */
	/* format P2 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */ 
	/* 1 0 1 0 brmask    gr            */
	if ((brmask >= 32) || (gr >= 128)) {
		return __UNW_INV_ARG_ERROR;
	}
	desc._fix[0] = 0xa0;
	desc._fix[0] |= ((brmask & 0x1e) >> 1);
	desc._fix[1] = 0x00;
	desc._fix[1] |= ((brmask & 0x01) << 7);
	desc._fix[1] |= gr;
	rsize = 2;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)&desc);

	return ret;
}



/* producer function to add a prologue spill_base descriptor */
__unw_error_t unwind_info_add_prologue_spill_base_info(__unw_info_t *info,
							__uint64_t pspoffset) {
	__unw_error_t ret = __UNW_OK;
	__unw_format_p7_t *desc;
	__uint64_t rsize, esize;
	char encoded[1+__UNW_ENCODING_SIZE];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* construct and add the spill_base descriptor */
	/* format P7 */
	/* 7 6 5 4 3 2 1 0                */
	/* 1 1 1 0 0 0 1 0 pspoff(LEB128) */
	encoded[0] = 0xe2;
	if ((esize = __leb128_encode((char *)&encoded[1],
				(__uint64_t)__UNW_ENCODING_SIZE,
				pspoffset)) == 0) {
		return __UNW_INTERNAL_ERROR;
	}

	desc = (__unw_format_p7_t *)&encoded;
	rsize = 1 + esize;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char *)desc);

	return ret;
}



/* producer function to add a prologue spill_mask descriptor */
__unw_error_t unwind_info_add_prologue_spill_mask_info(__unw_info_t *info,
						void *ptr, __uint64_t size) {
	__unw_error_t ret;
	char type[1];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* add descriptor */
	type[0] = 0xb8;
	ret = unwind_info_add_desc(1, type);
	if (ret != __UNW_OK) {
		return ret;
	}
	return unwind_info_add_desc(size, (char *)ptr);
}

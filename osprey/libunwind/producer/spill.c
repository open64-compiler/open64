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


/* translate reg enumeration to a,b,reg tuple */
static void
reg_enum_to_abreg(__UNW_REG_TYPE ureg, 
	__uint8_t *a, __uint8_t *b, __uint8_t *reg)
{
	if (ureg <= __UNW_R7) {
		*a = 0;
		*b = 0;
		*reg = (ureg - __UNW_R4) + 4;
	}
	else if (ureg <= __UNW_F31) {
		*a = 0;
		*b = 1;
		if (ureg <= __UNW_F5)
			*reg = (ureg - __UNW_F2) + 2;
		else
			*reg = (ureg - __UNW_F16) + 16;
	}
	else if (ureg <= __UNW_B5) {
		*a = 1;
		*b = 0;
		*reg = (ureg - __UNW_B1) + 1;
	}
	else {
		*a = 1;
		*b = 1;
		*reg = (ureg - __UNW_PRED);
	}
}

/* producer function to add a spill_psprel descriptor */
__unw_error_t 
unwind_info_add_spill_psprel_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint64_t offset) 
{
	__unw_error_t ret = __UNW_OK;
        __uint64_t rsize, esize1, esize2;
	__uint8_t a, b, reg;
        char encoded[2+(2*__UNW_ENCODING_SIZE)];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}
        /* check valid time argument */
        if (_current_region_total_size > 0 && when >= _current_region_total_size) {
                return __UNW_INV_SIZE_ERROR;
        }

	/* construct and add the spill_offset descriptor */
	/* format X1 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */
	/* 1 1 1 1 1 0 0 1 r a b reg       t(LEB128) spoff(ULEB128) */
	/* r = 0 */
	encoded[0] = 0xf9;
	reg_enum_to_abreg(ureg, &a, &b, &reg);
	encoded[1] = 0;
	encoded[1] |= (a << 6);
	encoded[1] |= (b << 5);
	encoded[1] |= reg;
	if ((esize1 = __leb128_encode((char *)&encoded[2],
		(__uint64_t)__UNW_ENCODING_SIZE,
		when)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	if ((esize2 = __leb128_encode((char *)&encoded[2+esize1],
		(__uint64_t)__UNW_ENCODING_SIZE,
		offset)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	rsize = 2 + esize1 + esize2;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char*) &encoded);
}

/* producer function to add a spill_sprel descriptor */
__unw_error_t 
unwind_info_add_spill_sprel_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint64_t offset) 
{
	__unw_error_t ret = __UNW_OK;
        __uint64_t rsize, esize1, esize2;
	__uint8_t a, b, reg;
        char encoded[2+(2*__UNW_ENCODING_SIZE)];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}
        /* check valid time argument */
        if (_current_region_total_size > 0 && when >= _current_region_total_size) {
                return __UNW_INV_SIZE_ERROR;
        }

	/* construct and add the spill_offset descriptor */
	/* format X1 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */
	/* 1 1 1 1 1 0 0 1 r a b reg       t(LEB128) spoff(ULEB128) */
	/* r = 1 */
	encoded[0] = 0xf9;
	reg_enum_to_abreg(ureg, &a, &b, &reg);
	encoded[1] = 0x80;
	encoded[1] |= (a << 6);
	encoded[1] |= (b << 5);
	encoded[1] |= reg;
	if ((esize1 = __leb128_encode((char *)&encoded[2],
		(__uint64_t)__UNW_ENCODING_SIZE,
		when)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	if ((esize2 = __leb128_encode((char *)&encoded[2+esize1],
		(__uint64_t)__UNW_ENCODING_SIZE,
		offset)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	rsize = 2 + esize1 + esize2;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char*) &encoded);
	return ret;
}

/* producer function to add a spill_reg descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_gr_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
        __uint64_t rsize, esize;
	__uint8_t a, b, reg;
        char encoded[3+1*__UNW_ENCODING_SIZE];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}
        /* check valid time argument */
        if (_current_region_total_size > 0 && when >= _current_region_total_size) {
                return __UNW_INV_SIZE_ERROR;
        }

	/* construct and add the spill_reg descriptor */
	/* format X2 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */ 
	/* 1 1 1 1 1 0 1 0 x a b reg       y treg    	   t(LEB128) */
	/* x = 0, y = 0 */
	encoded[0] = 0xfa;
	reg_enum_to_abreg(ureg, &a, &b, &reg);
	encoded[1] = 0;
	encoded[1] |= (a << 6);
	encoded[1] |= (b << 5);
	encoded[1] |= reg;
	encoded[2] = 0x0;
	encoded[2] |= treg;
	if ((esize = __leb128_encode((char *)&encoded[3],
		(__uint64_t)__UNW_ENCODING_SIZE,
		when)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	rsize = 3 + esize;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char*) &encoded);
	return ret;
}

/* producer function to add a spill_reg descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_fr_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_br_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg descriptor */
__unw_error_t 
unwind_info_add_restore_reg_info (__unw_info_t *info, 
	__uint64_t when,
	__UNW_REG_TYPE ureg)
{
	__unw_error_t ret = __UNW_OK;
        __uint64_t rsize, esize;
	__uint8_t a, b, reg;
        char encoded[3+1*__UNW_ENCODING_SIZE];

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}
        /* check valid time argument */
        if (_current_region_total_size > 0 && when >= _current_region_total_size) {
                return __UNW_INV_SIZE_ERROR;
        }

	/* construct and add the spill_reg descriptor */
	/* format X2 */
	/* 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 */ 
	/* 1 1 1 1 1 0 1 0 x a b reg       y treg    	   t(LEB128) */
	/* x = 0, y = 0, treg = 0 */
	encoded[0] = 0xfa;
	reg_enum_to_abreg(ureg, &a, &b, &reg);
	encoded[1] = 0;
	encoded[1] |= (a << 6);
	encoded[1] |= (b << 5);
	encoded[1] |= reg;
	encoded[2] = 0x0;
	if ((esize = __leb128_encode((char *)&encoded[3],
		(__uint64_t)__UNW_ENCODING_SIZE,
		when)) == 0) 
	{
		return __UNW_INTERNAL_ERROR;
	}
	rsize = 3 + esize;

	/* add descriptor */
	ret = unwind_info_add_desc(rsize, (char*) &encoded);
	return ret;
}

/* producer function to add a spill_psprel_p descriptor */
__unw_error_t 
unwind_info_add_spill_psprel_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint64_t offset) 
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_sprel_p descriptor */
__unw_error_t 
unwind_info_add_spill_sprel_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint64_t offset) 
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg_p descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_gr_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg_p descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_fr_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg_p descriptor */
__unw_error_t 
unwind_info_add_spill_reg_to_br_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg,
	__uint32_t treg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/* producer function to add a spill_reg_p descriptor */
__unw_error_t 
unwind_info_add_restore_reg_p_info (__unw_info_t *info, 
	__uint32_t qp,
	__uint64_t when,
	__UNW_REG_TYPE ureg)
{
	__unw_error_t ret = __UNW_OK;
	return __UNW_INV_OP_ERROR;
}

/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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
#include <strings.h>
#ifndef __GNUC__
#include <cmplrs/ia64/targ_isa_registers.h>
#else
#include <targ_isa_registers.h>
#endif
#include <sys/unwindP.h>
#include "unwind_producer.h"



#define MAX_GENERAL_REGISTERS		128
#define MAX_FP_REGISTERS		128
#define MAX_BRANCH_REGISTERS		8
#define MAX_PREDICATE_REGISTERS		64
#define MAX_APPLICATION_REGISTERS	128


static __UNW_REG_TYPE
reg_to_enum (__uint32_t regclass, __uint32_t regno)
{
  return __UNW_UNDEFINED_REG;
}

/* producer function to add a reg-to-reg unwind descriptor */
__unw_error_t unwind_info_add_prologue_info_reg(__unw_info_t *info,
						__uint32_t regclass_src,
						__uint32_t regno_src,
						__uint64_t when,
						__uint32_t regclass_dest,
						__uint32_t regno_dest) {
  return __UNW_INV_ARG_ERROR;
}

/* add body descriptor for reg->reg save */
__unw_error_t unwind_info_add_body_info_reg(__unw_info_t *info,
						__uint32_t regclass_src,
						__uint32_t regno_src,
						__uint64_t when,
						__uint32_t regclass_dest,
						__uint32_t regno_dest) 
{
  return __UNW_INV_ARG_ERROR;
}

__unw_error_t unwind_info_add_body_info_restore (__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when)
{
  return __UNW_INV_ARG_ERROR;
}


/* producer function to add a reg-to-spoffset unwind descriptor */
__unw_error_t unwind_info_add_prologue_info_sp_offset(__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when,
						__uint64_t spoffset) {
  return __UNW_INV_ARG_ERROR;
}



/* producer function to add a reg-to-pspoffset unwind descriptor */
__unw_error_t unwind_info_add_prologue_info_psp_offset(__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when,
						__uint64_t pspoffset) {
  return __UNW_INV_ARG_ERROR;
}

/* add body descriptor for reg->sp save */
__unw_error_t unwind_info_add_body_info_sp_offset(__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when,
						__uint64_t spoffset) 
{
  return __UNW_INV_ARG_ERROR;}

/* add body descriptor for reg->psp save */
__unw_error_t unwind_info_add_body_info_psp_offset(__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when,
						__uint64_t pspoffset) 
{
  return __UNW_INV_ARG_ERROR;
}

/* producer function to add a reg-to-fixedval unwind descriptor */
__unw_error_t unwind_info_add_prologue_info_fixed_value(__unw_info_t *info,
						__uint32_t regclass,
						__uint32_t regno,
						__uint64_t when,
						__uint64_t val) {
  return __UNW_INV_ARG_ERROR;
}

/* function to set the imask data */
__unw_error_t unwind_info_set_imask(__unw_info_t *info, __uint32_t flag,
							__uint64_t when) {
  return __UNW_OK;
}



/* function to get the imask data */
__unw_error_t unwind_info_get_imask(__unw_info_t *info, __uint32_t *flag,
							__uint64_t when) {
  return __UNW_OK;
}



/* function to flush the imask data */
__unw_error_t unwind_info_add_imask(__unw_info_t *info) {
  return __UNW_OK;
}

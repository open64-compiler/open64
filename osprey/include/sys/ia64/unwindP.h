
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

/*
 * ==========================================================================
 *
 * Module   : unwind.h -- IA64 unwind producer header file.
 *	      Normally used only by compiler, assembler
 *
 * ==========================================================================
 */

#ifndef __SYS_IA64_UNWINDP_H
#define __SYS_IA64_UNWINDP_H

#include <sys/ia64/unwind_ia64.h>

#ifdef __cplusplus
extern "C" {
#endif

/* producer API */

/* functionality for general operations */
__unw_error_t unwind_info_initialize(__unw_info_t **,
				__unw_addr_t /* start addr */,
				__unw_addr_t /* end addr */);
__unw_error_t unwind_info_finalize(__unw_info_t *);
__unw_error_t unwind_cleanup(void);

/* unwind table and/or unwind info dumping into elf sections */
__unw_error_t unwind_dump2elf(char *, uint64_t,
				char *, uint64_t, void *);

/* functionality for the unwind descriptors that record */
/* the (prologue/body) region headers */
__unw_error_t unwind_info_add_prologue_header(__unw_info_t *,
				uint64_t /* region size */);
__unw_error_t unwind_info_add_prologue_gr_header(__unw_info_t *,
				uint64_t /* region size */,
				char mask /* reg mask */,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_body_header(__unw_info_t *,
				uint64_t /* region size */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the stack pointer and/or the memory stack frame size */
__unw_error_t unwind_info_add_prologue_mem_stack_f_info(__unw_info_t *,
				uint64_t /* when */,
				uint64_t /* fixed frame size */);
__unw_error_t unwind_info_add_prologue_mem_stack_v_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_psp_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_psp_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the return pointer */
__unw_error_t unwind_info_add_prologue_rp_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_rp_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_rp_br_info(__unw_info_t *,
				uint32_t /* alternate br no */);
__unw_error_t unwind_info_add_prologue_rp_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_rp_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the previous function state register */
__unw_error_t unwind_info_add_prologue_pfs_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_pfs_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_pfs_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_pfs_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the preserved predicates */
__unw_error_t unwind_info_add_prologue_preds_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_preds_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_preds_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_preds_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the general, floating-point and branch registers */
__unw_error_t unwind_info_add_prologue_fr_mem_info(__unw_info_t *,
				uint32_t /* fr mask */);
__unw_error_t unwind_info_add_prologue_frgr_mem_info(__unw_info_t *,
				uint32_t /* gr mask */,
				uint32_t /* fr mask */);
__unw_error_t unwind_info_add_prologue_gr_gr_info(__unw_info_t *,
				uint32_t /* gr mask */,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_gr_mem_info(__unw_info_t *,
				uint32_t /* gr mask */);
__unw_error_t unwind_info_add_prologue_br_mem_info(__unw_info_t *,
				uint32_t /* br mask */);
__unw_error_t unwind_info_add_prologue_br_gr_info(__unw_info_t *,
				uint32_t /* br mask */,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_spill_base_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_spill_mask_info(__unw_info_t *,
				void * /* pointer to spill mask */,
				uint64_t /* size of spill mask */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the user NaT collection register */
__unw_error_t unwind_info_add_prologue_unat_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_unat_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_unat_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_unat_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the loop counter register */
__unw_error_t unwind_info_add_prologue_lc_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_lc_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_lc_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_lc_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the floating-point status register */
__unw_error_t unwind_info_add_prologue_fpsr_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_fpsr_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_fpsr_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_fpsr_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the primary unat collection */
__unw_error_t unwind_info_add_prologue_priunat_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_priunat_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_priunat_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_priunat_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality for the unwind descriptors in the prologue that record */
/* the state of the backing store */
__unw_error_t unwind_info_add_prologue_bsp_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_bsp_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_bsp_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_bsp_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);
__unw_error_t unwind_info_add_prologue_bspstore_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_bspstore_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_bspstore_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_bspstore_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);
__unw_error_t unwind_info_add_prologue_rnat_when_info(__unw_info_t *,
				uint64_t /* when */);
__unw_error_t unwind_info_add_prologue_rnat_gr_info(__unw_info_t *,
				uint32_t /* gr no */);
__unw_error_t unwind_info_add_prologue_rnat_psprel_info(__unw_info_t *,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_rnat_sprel_info(__unw_info_t *,
				uint64_t /* sp offset */);

/* functionality (overall) for the unwind descriptors in the prologue that record */
/* general-purpose info (most of the above functionality combined) */
__unw_error_t unwind_info_add_prologue_info_reg(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint32_t /* reg class */,
				uint32_t /* reg no */);
__unw_error_t unwind_info_add_prologue_info_sp_offset(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint64_t /* sp offset */);
__unw_error_t unwind_info_add_prologue_info_psp_offset(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint64_t /* psp offset */);
__unw_error_t unwind_info_add_prologue_info_fixed_value(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint64_t /* fixed value */);

/* functionality (overall) for the unwind descriptors in the body that record */
/* general-purpose info (most of the above functionality combined) */
__unw_error_t unwind_info_add_body_info_restore (__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */);
__unw_error_t unwind_info_add_body_info_reg (__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint32_t /* reg class */,
				uint32_t /* reg no */);
__unw_error_t unwind_info_add_body_info_sp_offset(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint64_t /* sp offset */);
__unw_error_t unwind_info_add_body_info_psp_offset(__unw_info_t *,
				uint32_t /* reg class */,
				uint32_t /* reg no */,
				uint64_t /* when */,
				uint64_t /* psp offset */);


/* functionality for the unwind descriptors in the body */
/* that record the epilogue */
__unw_error_t unwind_info_add_body_epilogue_info(__unw_info_t *,
				uint64_t /* when */,
				uint64_t /* count for nested */
					   /* shrink-wrap regions */);

/* functionality for the unwind descriptors in the body */
/* that record body label state */
__unw_error_t unwind_info_add_body_label_state_info(__unw_info_t *,
				uint64_t /* label */);

/* functionality for the unwind descriptors in the body */
/* that record body copy state */
__unw_error_t unwind_info_add_body_copy_state_info(__unw_info_t *,
				uint64_t /* label */);

/* functionality to add personality routine */
__unw_error_t unwind_info_add_personality_routine_info(__unw_info_t *,
		__unw_addr_t /* gp-relative offset of personality routine */,
		uint32_t /* ehandler flag */, uint32_t /* uhandler flag */);

/* functionality to add language-specific data */
__unw_error_t unwind_info_add_language_specific_info(__unw_info_t *,
				void * /* pointer to data */,
				uint64_t /* size of data */);


#ifdef __cplusplus
}
#endif

#endif

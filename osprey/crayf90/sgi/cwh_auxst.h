/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_auxst.h
 * $Revision: 1.5 $
 * $Date: 05/01/13 22:11:47-08:00 $
 * $Author: scorrell@soapstone.internal.keyresearch.com $
 * $Source: crayf90/sgi/SCCS/s.cwh_auxst.h $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Exports interfaces from cwh_stab.c (symbol tables).
 *              Most descriptions are in cwh_stab.{c,i}
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_AUXST_INCLUDED
#define CWH_AUXST_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_auxst.h $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

/* de/allocation */
extern void   cwh_auxst_register_table(void);
extern void   cwh_auxst_un_register_table(void);
extern void   cwh_auxst_alloc_container_table(void);
extern void   cwh_auxst_clear_per_PU(void) ;
extern void   cwh_auxst_free(void) ;
extern void   cwh_auxst_free_list (LIST ** lp) ;

extern void   cwh_auxst_clear(ST *st);


/* ----------------------------------------------------
   TAGS for processing lists of additional information 
    used as flags to AUXST lookup routine.              
*/

enum list_name {
  l_COMLIST,               /* STs in a COMMON ordered by offset          */
  l_ALTENTRY,              /* STs of alternate entry points              */
  l_RETURN_TEMPS,          /* STs of alternate entry return temps        */
  l_NAMELIST,              /* STs of Namelist items                      */
  l_SPLITLIST,             /* STs of split commons, for pragmas          */
  l_EQVLIST,		   /* STs in Equivalence                         */
  l_DST_COMLIST,	   /* COMMON STs (Global) for DST info in PU     */
  l_DST_PARMLIST	   /* Parameter STs (Global) for DST info in PU  */
#ifdef KEY /* Bug 5271 */
  , l_PU_COMLIST           /* STs for vars within a COMMON for current   */
                           /* PU, ordered by offset                      */
#endif /* KEY Bug 5271 */
};


/* ----------------------------------------------------
    enum for flags in AUXST - used as mask 
    cwh_auxst.i has details, macros below,
*/

enum flags_a { 
	f_RSLTTMP = 0x01,     /* fn has rslt temp as 1st arg      */
	f_ELEM    = 0x02,     /* is elemental function            */
	f_ALTENT  = 0x04,     /* is alternate entry point         */
	f_ALTTY   = 0x08,     /* all alternate entries share a TY */
	f_VISITED = 0x10,     /* visited on IO walk               */
	f_NONCONT = 0x20,     /* is non-contiguous                */
	f_AUTO_OR_CPTR = 0x40,/* Auto or Cray Pointer             */
	f_F90_PTR = 0x80,     /* F90 Pointer                      */
	f_MODULE  = 0x100,    /* Common represents module data    */
	f_XP_COPY = 0x200,    /* Have emitted XPRAGMA COPYIN      */
	f_IS_TMP  = 0x400,    /* is a compiler temp               */
	f_ALLOC   = 0x800,    /* Allocatable                      */
	f_ASSUMED = 0x1000,   /* Assumed shape                    */
	f_A_SIZE  = 0x2000,   /* Assumed size                     */
	f_IS_RSLTTMP = 0x4000 /* is the fn rslt temp (1st arg)    */
} ;


#define Set_ST_auxst_has_rslt_tmp(st,val) cwh_auxst_set_flag(st,f_RSLTTMP,val)
#define ST_auxst_has_rslt_tmp(st) cwh_auxst_read_flag(st,f_RSLTTMP)

#define Set_ST_auxst_is_rslt_tmp(st,val) cwh_auxst_set_flag(st,f_IS_RSLTTMP,val)
#define ST_auxst_is_rslt_tmp(st) cwh_auxst_read_flag(st,f_IS_RSLTTMP)

#define Set_ST_auxst_is_elemental(st,val) cwh_auxst_set_flag(st,f_ELEM,val)
#define ST_auxst_is_elemental(st) cwh_auxst_read_flag(st,f_ELEM)

#define Set_ST_auxst_is_altentry(st,val) cwh_auxst_set_flag(st,f_ALTENT,val)
#define ST_auxst_is_altentry(st) cwh_auxst_read_flag(st,f_ALTENT)

#define Set_ST_auxst_altentry_shareTY(st,val) cwh_auxst_set_flag(st,f_ALTTY,val)
#define ST_auxst_altentry_shareTY(st) cwh_auxst_read_flag(st,f_ALTTY)

#define Set_ST_auxst_visited(st,val) cwh_auxst_set_flag(st,f_VISITED,val)
#define ST_auxst_visited(st) cwh_auxst_read_flag(st,f_VISITED)

#define Set_ST_auxst_is_non_contiguous(st,val) cwh_auxst_set_flag(st,f_NONCONT,val)
#define ST_auxst_is_non_contiguous(st) cwh_auxst_read_flag(st,f_NONCONT)

#define Set_ST_auxst_is_auto_or_cpointer(st,val) cwh_auxst_set_flag(st,f_AUTO_OR_CPTR,val)
#define ST_auxst_is_auto_or_cpointer(st) cwh_auxst_read_flag(st,f_AUTO_OR_CPTR)

#define Set_ST_auxst_is_f90_pointer(st,val) cwh_auxst_set_flag(st,f_F90_PTR,val)
#define ST_auxst_is_f90_pointer(st) cwh_auxst_read_flag(st,f_F90_PTR)

#define Set_ST_auxst_is_module_data(st,val) cwh_auxst_set_flag(st,f_MODULE,val)
#define ST_auxst_is_module_data(st) cwh_auxst_read_flag(st,f_MODULE)

#define Set_ST_auxst_xpragma_copyin(st,val) cwh_auxst_set_flag(st,f_XP_COPY,val)
#define ST_auxst_xpragma_copyin(st) cwh_auxst_read_flag(st,f_XP_COPY)

#define Set_ST_auxst_is_tmp(st,val) cwh_auxst_set_flag(st,f_IS_TMP,val)
#define ST_auxst_is_tmp(st) cwh_auxst_read_flag(st,f_IS_TMP)

#define Set_ST_auxst_is_allocatable(st,val) cwh_auxst_set_flag(st,f_ALLOC,val)
#define ST_auxst_is_allocatable(st) cwh_auxst_read_flag(st,f_ALLOC)

#define Set_ST_auxst_is_assumed_shape(st,val) cwh_auxst_set_flag(st,f_ASSUMED,val)
#define ST_auxst_is_assumed_shape(st) cwh_auxst_read_flag(st,f_ASSUMED)

#define Set_ST_auxst_is_assumed_size(st,val) cwh_auxst_set_flag(st,f_A_SIZE,val)
#define ST_auxst_is_assumed_size(st) cwh_auxst_read_flag(st,f_A_SIZE)

extern void   cwh_auxst_set_flag(ST * st,enum flags_a f, BOOL val) ;
extern BOOL   cwh_auxst_read_flag(ST * st,enum flags_a f);

/* Forward reference, defined in cwh_data.cxx */

struct data_info_s;
extern void   Set_ST_auxst_data_info(ST *st, data_info_s * data_info);
extern data_info_s * ST_auxst_data_info(ST *st);


/* individual items */

extern ITEM * cwh_auxst_add_to_list(LIST ** lp, ST *st, BOOL order);
extern void   cwh_auxst_add_list(ST * parent, LIST *l, enum list_name list) ;
extern LIST * cwh_auxst_get_list(ST * st, enum list_name list);
extern void   cwh_auxst_add_item(ST * parent, ST *st, enum list_name list);
extern ITEM * cwh_auxst_next_element(ST * parent, ITEM *i, enum list_name list) ;
extern BOOL   cwh_auxst_read_flag(ST * st, enum flags_a f) ;
extern void   cwh_auxst_set_flag(ST * st, enum flags_a f, BOOL val);
extern ST *   cwh_auxst_find_item(LIST *l, const char * name);
#ifdef KEY /* Bug 5271 */
extern void cwh_clear_PU_common_list(ST *st);
#endif /* KEY Bug 5271 */

/* procedure details */

extern void   cwh_auxst_alloc_proc_entry(ST *st,INT32 num_dum_args, TY_IDX ret_type);
extern void   cwh_auxst_patch_proc(TY_IDX rty_idx);
extern BOOL   cwh_auxst_find_dummy(ST * arg);
extern INT16  cwh_auxst_num_dummies(ST * entry);
extern ST **  cwh_auxst_arglist(ST * entry);
extern ST *   cwh_auxst_find_dummy_len(ST * arg);
extern void   cwh_auxst_add_dummy(ST * dummy, BOOL result);
extern void   cwh_auxst_set_tylist(ST *en);

/* sundry utilities */

extern ST *      cwh_auxst_cri_pointee(ST * ptr, ST * pointee);
extern PREG_det  cwh_auxst_distr_preg(ST * st);
extern USRCPOS   cwh_auxst_srcpos_val(ST * st);
extern USRCPOS * cwh_auxst_srcpos_addr(ST * st) ;
extern WN *      cwh_auxst_pragma(ST * ptr, WN * wn = NULL);
#ifdef KEY /* Bug 4901 */
extern void	 cwh_auxst_clear_stem_name(ST *st);
#endif /* KEY Bug 4901 */
extern char *    cwh_auxst_stem_name(ST * st, char * name = NULL) ;
INT32 *          cwh_auxst_assign_id(SYMTAB_IDX level, LABEL_IDX idx) ;

/* dump routines */

extern void cwh_auxst_dump (ST * st);
extern void cwh_auxst_dump_list (LIST * l, BOOL verbose);

#endif /* CWH_AUXST_INCLUDED */

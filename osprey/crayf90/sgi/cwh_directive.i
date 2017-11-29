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

int parallel_do_count;

/* table to map asserts to whirl nodes */
/* the MIPS_ASSERT_* are defined in fe90/i_cvrt.h, this table must be sync-ed */
static struct {
  int fe_enum;
  WN_PRAGMA_ID wn_pragma_id;
  } map_asserts[] = {
/* 0 */  { 0, WN_PRAGMA_UNDEFINED } /* 0'th index doesnt exist */,
/* 1 */  {MIPS_ASSERT_ARGUMENTALIASING, WN_PRAGMA_KAP_ASSERT_ARGUMENT_ALIASING},
/* 2 */  {MIPS_ASSERT_NOARGUMENTALIASING, WN_PRAGMA_KAP_ASSERT_NO_ARGUMENT_ALIASING},
/* 3 */  {MIPS_ASSERT_BOUNDSVIOLATIONS, WN_PRAGMA_KAP_ASSERT_BOUNDS_VIOLATIONS},
/* 4 */  {MIPS_ASSERT_NOBOUNDSVIOLATIONS, WN_PRAGMA_KAP_ASSERT_NOBOUNDS_VIOLATIONS},
/* 5 */  {MIPS_ASSERT_CONCURRENTCALL, WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL},
/* 6 */  {MIPS_ASSERT_NOCONCURRENTCALL, WN_PRAGMA_KAP_ASSERT_NO_CONCURRENT_CALL},
/* 7 */  {MIPS_ASSERT_NORECURRENCE, WN_PRAGMA_KAP_ASSERT_NORECURRENCE},
/* 8 */  {MIPS_ASSERT_DOPREFER, WN_PRAGMA_KAP_ASSERT_DOPREFER},
/* 9 */  {MIPS_ASSERT_EQUIVALENCEHAZARD, WN_PRAGMA_KAP_ASSERT_EQUIVALENCE_HAZARD},
/* 10 */ {MIPS_ASSERT_NOEQUIVALENCEHAZARD, WN_PRAGMA_KAP_ASSERT_NOEQUIVALENCE_HAZARD},
/* 11 */ {MIPS_ASSERT_LASTVALUENEEDED, WN_PRAGMA_KAP_ASSERT_LAST_VALUE_NEEDED},
/* 12 */ {MIPS_ASSERT_LASTVALUESNEEDED, WN_PRAGMA_KAP_ASSERT_LAST_VALUE_NEEDED},
/* 13 */ {MIPS_ASSERT_NOLASTVALUENEEDED, WN_PRAGMA_KAP_ASSERT_NOLAST_VALUE_NEEDED},
/* 14 */ {MIPS_ASSERT_NOLASTVALUESNEEDED, WN_PRAGMA_KAP_ASSERT_NOLAST_VALUE_NEEDED},
/* 15 */ {MIPS_ASSERT_PERMUTATION, WN_PRAGMA_KAP_ASSERT_PERMUTATION},
/* 16 */ {MIPS_ASSERT_RELATION, WN_PRAGMA_KAP_ASSERT_RELATION},
/* 17 */ {MIPS_ASSERT_NOSYNC, WN_PRAGMA_KAP_ASSERT_NOSYNC},
/* 18 */ {MIPS_ASSERT_TEMPORARIESFORCONSTANTARGUMENTS, WN_PRAGMA_KAP_ASSERT_TEMPORARIES_FOR_CONSTANT_ARGUMENTS},
/* 19 */ {MIPS_ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS, WN_PRAGMA_KAP_ASSERT_NOTEMPORARIES_FOR_CONSTANT_ARGUMENTS},
/* 20 */ {MIPS_ASSERT_DO, WN_PRAGMA_KAP_ASSERT_DO},
/* 21 */ {MIPS_ASSERT_BENIGN, WN_PRAGMA_KAP_ASSERT_BENIGN},
/* 22 */ {MIPS_ASSERT_DEPENDENCE, WN_PRAGMA_KAP_ASSERT_DEPENDENCE},
/* 23 */ {MIPS_ASSERT_FREQUENCY, WN_PRAGMA_KAP_ASSERT_FREQUENCY},
/* 24 */ {MIPS_ASSERT_IGNOREANYDEPENDENCES, WN_PRAGMA_KAP_ASSERT_IGNORE_ANY_DEPENDENCE},
/* 25 */ {MIPS_ASSERT_IGNOREANYDEPENDENCE, WN_PRAGMA_KAP_ASSERT_IGNORE_ANY_DEPENDENCE},
/* 26 */ {MIPS_ASSERT_IGNOREASSUMEDDEPENDENCES, WN_PRAGMA_KAP_ASSERT_IGNORE_ASSUMED_DEPENDENCE},
/* 27 */ {MIPS_ASSERT_IGNOREASSUMEDDEPENDENCE, WN_PRAGMA_KAP_ASSERT_IGNORE_ASSUMED_DEPENDENCE},
/* 28 */ {MIPS_ASSERT_NOINTERCHANGE, WN_PRAGMA_KAP_ASSERT_NO_INTERCHANGE},
/* 29 */ {MIPS_ASSERT_USECOMPRESS, WN_PRAGMA_KAP_ASSERT_USE_COMPRESS},
/* 30 */ {MIPS_ASSERT_USEEXPAND, WN_PRAGMA_KAP_ASSERT_USE_EXPAND},
/* 31 */ {MIPS_ASSERT_USECONTROLLEDSTORE, WN_PRAGMA_KAP_ASSERT_USE_CONTROLLED_STORE},
/* 32 */ {MIPS_ASSERT_USEGATHER, WN_PRAGMA_KAP_ASSERT_USE_GATHER},
/* 33 */ {MIPS_ASSERT_USESCATTER, WN_PRAGMA_KAP_ASSERT_USE_SCATTER}
};


static WN * do_loop_directive_block=NULL;
static void cwh_directive_pragma_to_region(WN * prag, WN * region) ;
#ifndef KEY
static void cwh_directive_set_PU_flags(BOOL nested);
#endif
static void cwh_directive_set_LNO_flags(void);
static void cwh_directive_pop_and_nowait( BOOL nowait, BOOL is_omp);
static void cwh_directive_add_pragma_to_loop(WN * wn, BOOL is_omp);
static void cwh_directive_work_dist(INT32 work_dist, INTPTR work_dist_opnd) ;
static void cwh_directive_load_value_pragma(INTPTR item, WN_PRAGMA_ID pragma, BOOL is_omp = FALSE);
static void cwh_directive_fill_align(INT32 count,INT32 C_value, WN_PRAGMA_ID pragma ) ;




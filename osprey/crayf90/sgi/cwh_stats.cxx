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
 * Module: cwh_stats.c
 * $Revision: 
 * $Date$
 * $Author$
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Accumulates and prints statistics about the
 *              conversion. Essentially a bunch of counters,
 *              and associated strings.
 *
 *              Counters for TYs are defined,
 *              but there's no reason others couldn't be
 *              added. Enums and macros in cwh_stats.h
 *              define the counter groups and cwh_stats.i contains
 *              the local definitions and strings.
 *
 * ====================================================================
 * ====================================================================
 */

static char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


/* sgi includes */

#include "defs.h"
#include "stab.h"
#include "tracing.h"

/* conversion includes */

#include "cwh_stats.h"
#include "cwh_stats.i"

/*================================================================
 *
 * cwh_stats_print
 *
 * Top level routine to print stats. Calls one routine 
 * for each group of items to print. Pass in the PU's ST.
 *
 * Only TY details are printed just now.
 *
 * invoked via -ttIRB:16 
 *
 * ================================================================
 */
extern void 
cwh_stats_print(ST * st) 
{
  CTR * cp ;

  fprintf (TFile,"\n%s (last TY id %d) ",ST_name(st),SYMTAB_last_type_id(Global_Symtab));

  if (ST_pu_is_nested_func(st)) 
    fprintf (TFile," (internal) ");

  fprintf (TFile," \n \n");

  cwh_stats_print_details(&ty_c) ;

}

/*================================================================
 *
 * cwh_stats_print_details
 *
 * Walk through array  of counters 
 *
 * a) print the values
 * b) save current values to compute increments per PU.
 * c) sum the values.
 *
 * ================================================================
 */
static void 
cwh_stats_print_details( CTR * cp ) 
{

  int sum,i,val ;
  sum = 0 ;

  fprintf (TFile,CTR_misc_str(cp,0));

  for (i = 0 ; i < CTR_num(cp) ; i ++) {

    val = CTR_count(cp,i);

    if (CTR_add_to_sum(cp,i))
      sum += val ;

    if (val != 0) 
      fprintf (TFile,CTR_str(cp,i),val,val-CTR_saved(cp,i));

    CTR_saved(cp,i) = val;
  
  }

  fprintf (TFile,CTR_misc_str(cp,1));
  fprintf (TFile,CTR_misc_str(cp,2),sum,sum-CTR_saved_sum(cp)) ;
  CTR_saved_sum(cp) = sum;

}


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


/* stub definitions for the PQS routines */

#include "defs.h"
#include "pqs_cg.h"

void PQSCG_init(BB * first_bb){}
void PQSCG_reinit(BB * first_bb){}
void PQSCG_term(void){}
BOOL PQSCG_pqs_valid(void){return FALSE;}     
 
BOOL PQSCG_is_disjoint(PQS_TN tn1, PQS_TN tn2){return FALSE;}
BOOL PQSCG_is_disjoint(PQS_TN_SET &tns1, PQS_TN_SET &tns2){return FALSE;}


BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN tn2){return FALSE;}
BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN_SET &tns2){return FALSE;}
BOOL PQSCG_is_subset_of (PQS_TN_SET &tns1, PQS_TN_SET &tns2){return FALSE;}

void PQSCG_copy_tn_map(PQS_TN tn_out, PQS_TN tn_in){}
void PQSCG_add_instruction(PQS_OP op){}
BOOL PQSCG_sets_results_if_qual_true(PQS_OP op) {return TRUE;}

MEM_POOL PQS_mem_pool;
BOOL PQS_disabled;


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


#ifndef PQS_CG_included
#define PQS_CG_included
/* 
   This header contains the external interfaces for the Predicate Query System
   parts which are related to the interaction with the code generator. 

*/

#include "pqs_defs.h"
#include "bb.h"
#include "tn.h"
#include "op.h"

/*================================================================
//================================================================
//================================================================

Sets of TNs.

//================================================================
*/

struct lt_tn {
   inline bool operator()(const PQS_TN t1, const PQS_TN t2) const {
    return TN_number(t1) < TN_number(t2);
   }
};

typedef PQS_SET<PQS_TN,lt_tn> PQS_TN_SET;
typedef PQS_SET<PQS_TN,lt_tn>::set_type PQS_TN_SET_TYPE;

//
// These are the query routines the PQS provides to CG.
// These can be used, or else the member functions in PQS_MANAGER can be used.
// These functions use the "official" PQS_MANAGER, whatever that is. 
//

/*================================================================
  Inititialization and termination
  
  void PQSCG_init(BB * first_bb)
  
  Initialize everything. first_bb and its successors are walked using FOR_ALL_BBLIST_ITEMS
  and the internal data structures are built. If the PQS is already initialized, then nothing
  is done. 

  void PQSCG_reinit(BB * first_bb)
    Simply call PQSCG_term() followed by PQSCG_init(first_bb)
  
  void PQSCG_term(void)
  
  Destroy the current PQS_MANAGER.

  BOOL PQSCG_pqs_valid(void) 
  
  Returns TRUE if the current CG PQS_MANAGER is available to ask questions about.
  
  ================================================================  
*/

extern void PQSCG_init(BB * first_bb);
extern void PQSCG_reinit(BB * first_bb);
extern void PQSCG_term(void);
extern BOOL PQSCG_pqs_valid(void);
     
/*================================================================
  PQSCG_is_disjoint returns TRUE if it can be proven that the two TNs can
  never be true simultaneously. They are allowed to be false
  simultaneously, and if nothing can be deduced, it also returns FALSE.
  
  The set version returns TRUE if no element of set 1 can be true if any element
  of set 2 is true (and vice versa)
  ================================================================
*/
 
extern BOOL PQSCG_is_disjoint(PQS_TN tn1, PQS_TN tn2);
extern BOOL PQSCG_is_disjoint(PQS_TN_SET &tns1, PQS_TN_SET &tns2);

/*================================================================
  PQSCG_is_subset_of returns TRUE if it is the case that whenever any
  element of the first argument is true, some element of the second
  argument is guaranteed to be true. This can be pretty useful. For example
  is_subset_of (P0, {P2,P3}) if true indicates that one of P2 or P3 is
  always true. This can be used to implement a killed query, where the
  first argument is the TN of the store we want to enquire about, and the
  second argument (the set) is the set of stores we want to make sure kills
  the first value.
  ================================================================
*/
extern BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN tn2);
extern BOOL PQSCG_is_subset_of (PQS_TN tn1, PQS_TN_SET &tns2);
extern BOOL PQSCG_is_subset_of (PQS_TN_SET &tns1, PQS_TN_SET &tns2);

/*================================================================
  PQSCG_copy_tn_map(PQS_TN tn_out, PQS_TN tn_in) copies the PQS map from
  tn_in to tn_out such that tn_out will have the same PQS properties as 
  tn_in.
  ================================================================ 
*/

extern void PQSCG_copy_tn_map(PQS_TN tn_out, PQS_TN tn_in);

/*================================================================
  PQSCG_add_instruction(PQS_OP op) adds a predicate defining instruction to the 
  PQS data structures. 
  ================================================================ 
*/

extern void PQSCG_add_instruction(PQS_OP op);


/*================================================================
  PQSCG_sets_results_if_qual_true(PQS_OP op)
  If it is possible for the qualifying predicate to be true and yet the results not be set in an 
  operation, then return FALSE. Most instructions (except for a few of the compares) return TRUE
  ================================================================
*/

extern BOOL PQSCG_sets_results_if_qual_true(PQS_OP op);

// Mempool
extern MEM_POOL PQS_mem_pool;


// Flag for forcing PQS off
extern BOOL PQS_disabled;

#endif








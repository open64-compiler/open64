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
* Module: opt_alias_interface.h
* $Revision: 1.2 $
* $Date: 02/11/07 23:41:38-00:00 $
* $Author: fchow@keyresearch.com $
* $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_alias_interface.h $
*
* Revision history:
*  07-APR-95 lo - spilt from opt_alias.h
*
* Description:
*
* ====================================================================
* ====================================================================
*/

#ifndef opt_alias_interface_INCLUDED
#define opt_alias_interface_INCLUDED	"opt_alias_interface.h"
#ifdef _KEEP_RCS_ID
static char *opt_alias_interfacercs_id = 	opt_alias_interface_INCLUDED"$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */


/* ========================================================================
*
*  ALIAS ANALYSIS INTERFACE
*  ------------------------
*
*  The alias information is generated in the preopt/wopt phase.
*  The consumers are IPA, LNO, and CG.  The information is passed in memory
*  for IPA and LNO.  The information is emitted into the WHIRL IR
*  and passed through .B file to CG.
*  [ The previous sentence is a lie. The information is passed in
*    memory to CG as well. -- RK 981106 ]
*
*
*  ALIAS_MANAGER
*  -------------
*  All the alias data is managed by the ALIAS_MANAGER.
*  The alias context is used to identify the alias rule set to 
*  be applied.  See opt_alias_rule.h for details about 
*  ALIAS_CONTEXT.
*
*  struct ALIAS_MANAGER *Create_Alias_Manager(void)
*    Create an alias manager.  The alias manager will have its
*    own memory pool and manage the memory used by the alias information
*    itself.
*
*  void Delete_Alias_Manager(struct ALIAS_MANAGER *)
*     Destroy the alias manager.  Free the MEM_POOL used.
*
*  ALIAS_CONTEXT Get_Default_Alias_Context(struct ALIAS_MANAGER *)
*     Obtain the current alias context.
*
*  void Set_Alias_Context(struct ALIAS_MANAGER *, ALIAS_CONTEXT);
*     Set up the current alias context.
*
*  void Reset_Alias_Context(struct ALIAS_MANAGER *);
*     Revert to the default alias context.
*
*
*  Alias analysis functions
*  ------------------------
*
*  The alias analysis function return ALIAS_RESULT (one of the following
*  three values).
*  
*    NOT_ALIASED:           two memory operations are not aliased.
*    POSSIBLY_ALIASED:      we can't prove the memory operations are not aliased.
*    SAME_LOCATION:         two memory operations are aliased and access exactly
*                           the same memory locations.  the size of memory accessed
*                           must be the same too.
*  
*  WARNING:  Alias analyis ALWAYS RETURN conservatively answers.
*  Even though two memory operations are not aliased, the analysis may not be
*  powerful enough to determine that.  It is correct to return POSSILBY_ALIASED.
*  Similarly, for two memory operations that access the same location, it is 
*  correct for the alias analyzer to return POSSIBLY_ALIASED.
*
*  ALIAS_RESULT Aliased(const struct ALIAS_MANAGER *, const WN *wn1, const WN *wn2)
*     Check whether the memory accessed by wn1 and wn2 overlaps.
*
*  ALIAS_RESULT Overlapped_base(const struct ALIAS_MANAGER *, const WN *wn1, const WN *wm2)
*     Ignore the context-sensitive offset information from wn1 and wn2. 
*     Check whether the memory accessed by wn1 and wn2 overlaps.
*
*  Example:
*     do i
*        a[i]
*        a[i+1]
*  
*   Aliased(manager, "a[i]", "a[i+1]") return NOT_ALIASED because
*   context-sensitive analysis MAY determine that a[i] and a[i+1] are both
*   based on &a[i].  By applying the Offset rule, a[i] and a[i+1] are said
*   to be not aliased.
*  
*   Notice that this is only true for a particular iteration.  The a[i+1]
*   is aliased to the *p of the following iteration.   LNO requested to
*   return POSSIBLY_ALIASED for a[i] and a[i+1] because they really care
*   about the dependence across all iterations.
*
*   The function Overlapped_base() is written to provide this functionality.
*   It always return ALIASED if the base is not a constant address.
*   It ignores the offset information derived from context-sensitive analysis.
*   Therefore, for both a[i] and a[i+1], the offset is undetermined.
*   However, for a[1] or a[2], the offset is determined becuase it can be derived
*   from the tree underneath the load/store operation.
*  
*     
*   ALIAS_RESULT Aliased_with_region(const struct ALIAS_MANAGER *, 
*           WN *call_or_region, WN *wn, READ_WRITE);
*      Check if the wn operations has dependence with a REGION.
*      CALL is regarded a simple region.  READ_WRITE specifies
*      the dependence checked (READ dependence, WRITE dependence, or both).
*      FORWARD_BARRIER/BACKWARD_BARRIER/DEALLOCA are another form or REGION.
*
*
*   BOOL Homing_Load( const struct ALIAS_MANAGER *, const WN *load_wn )
*   void Set_Homing_Load( struct ALIAS_MANAGER *, WN *load_wn )
*   void Reset_Homing_Load( struct ALIAS_MANAGER *, WN *load_wn )
*
*     Determine if the load is from a variable's home location to be stored
*     into that var's preg.  
*
*   BOOL Homing_Store( const struct ALIAS_MANAGER *, const WN *store_wn )
*   void Set_Homing_Store( struct ALIAS_MANAGER *, WN *store_wn )
*   void Reset_Homing_Store( struct ALIAS_MANAGER *, WN *store_wn )
*
*     Determine if the store is to a variable's home location and the value
*     being stored is that var's preg.
*
*  Lowering and Unrolling Support
*  ------------------------------
*
*  The alias package provides basic support for the update of
*  alias information as new memory operations are created during
*  lowering and unrolling.  The support is minimal so that correct
*  alias analysis is possible.  However, re-analyze the unrolled memop
*  is needed to provide a better solution. 
*  In fact, the Aliased_Memop() in cgprep.c of Ragnarok handle this situation
*  by examining the INS.
*
*  void Copy_alias_info(const struct ALIAS_MANAGER *, WN *wn1, WN *wn2)
*     The alias information of wn1 is transfered to wn2.  This will be used
*     by lowering.  For example, LDID a  --->   ILOAD (LDA a).
*     An iload is created and it should have identical alias behavior as the
*     LDID.
*
*  void Duplicate_alias_info(struct ALIAS_MANAGER *, 
*         const WN *wn1, WN *wn2)
*     The alias information of wn1 is duplicated to wn2, and some updating
*     both wn1 and wn1 to reduce the strength of alias analysis.
*
*  void Create_vector_alias(struct ALIAS_MANAGER *, WN *wn1, WN *wn2)
*     The alias information of wn1 (accessing an element) is
*     copied to wn2 (accessing the entire array).     
*
*  BOOL Valid_alias(const struct ALIAS_MANAGER *am, WN *wn);
*     Return true if the wn has a valid alias mapping
*
*  Duplicate_alias_info() is used by unrolling.  As CG unrolls a loop, 
*  it duplicates the memops.  The duplicated memops should have similar, but
*  not idential alias behavior as the original because
*  the alias information does not apply across iterations.
*
*  Notice that If the address of the memory operation is a loop invariant, then
*  the unroller should use Copy_alias_info() to transfer the alias information
*  in order to preserve all the useful alias information.
*
*  LNO & Lowerer Support
*  ---------------------
*
*  This routines update the alias information associated with newly created WHIRL nodes.
*
*  void Create_alias(struct ALIAS_MANAGER *, WN *wn)
*    Create a new alias id corresponding to WHIRL load/store.
*
*  void Create_local_alias(struct ALIAS_MANAGER *, WN *wn)
*    Create a new alias id corresponding to the ST in the WHIRL node.
*    Assume the ST is a local var, and not address taken.
*
*  void Create_global_alias(struct ALIAS_MANAGER *, ST *, WN *ldid, WN *iload)
*    The ST is a global variable. 
*    Create a new alias id corresponding to LDID ST to the ldid WHIRL node.
*    Create a new alias id corresponding to ILOAD (LDID ST) to the iload WHIRL node.
*
*  void Create_formal_alias(struct ALIAS_MANAGER *, ST *, WN *formal_addr, WN *formal)
*    The ST in formal_addr must be SCLASS_FORMAL.
*    Assume FORTRAN alias rule:  formals are not aliased with other variables.
*    Create alias ids for both the variable containing the formal and the formal.
*
*  void Create_unique_pointer_alias(struct ALIAS_MANAGER *, ST *, WN *ldid, WN *iload)
*    ST is the symbol that points to this chunk memory exclusively.
*    ST must have the PT_TO_UNIQUE_MEM bit set.
*    Create a new alias id corresponding to LDID ST to the ldid WHIRL node.
*    Create a new alias id corresponding to ILOAD (LDID ST) to the iload WHIRL node.
*
*  void Create_lda_array_alias(struct ALIAS_MANAGER *am, WN *lda, WN *iload)
*    Create a new alias id corresponding to the array access.
*
*  void Erase_Restricted_Mapping(WN *wn)
*    Erase the restricted pointer stored in the 'restrict map'.
*    Should be called when an array is localized.
*
*  void Note_Invalid_Based_Symbol(const ST *st)
*    Mark the symbol "st" as an invalid based_sym in the restricted
*    map. This should be called when an array is distribute/reshaped
*    by LNO because memory operations based on the symbol are now
*    based on another symbol introduced during the distribute/reshape
*    operation.
*
*  void Note_Invalid_IP_Alias_Class(const WN *wn)
*    Mark the interprocedural alias class of "wn" as invalid. This
*    should be called when an array accessed by "wn" is equivalenced
*    by LNO during Equivalence_arrays, because the alias behavior of
*    the array as seen by IPA is no longer valid.
*
*  void Invalidate_Persistent_Alias_Info(ALIAS_MANAGER *, WN *);
*    Removes those items marked as invalid by
*    Note_Invalid_Based_Symbol and Note_Invalid_IP_Alias_Class. This
*    should be called at the end of LNO, after Copy_Restricted_Map is
*    used to move the restricted map entries to the new WHIRL tree
*    from the alias manager.
*
*  ALIAS_RESULT ALIAS_MANAGER::Aliased(WN *wn, const POINTS_TO *pt);
*  ALIAS_RESULT ALIAS_MANAGER::Aliased(const POINTS_TO *pt, WN *wn);
*    Determine if a WHIRL load/store is aliased with a POINTS_TO.
*
*  ALIAS_RESULT ALIAS_MANAGER::Aliased(const POINTS_TO *pt1, const POINTS_TO *pt2);
*    Determine if two POINTS_TO are aliased.
*
*  POINTS_TO(ST *st, BOOL indirect = FALSE) 
*  POINTS_TO(ST *st, INT64 ofst, INT64 size, BOOL indirect = FALSE) 
*    The ALIAS_MANAGER::Aliased() routine takes POINTS_TO.  The POINTS_TO
*    can be constructed using these constructors.   Set indirect to
*    FALSE for regular variables.  Set indirect to TRUE when the ST is 
*    a pointer and you want to construct a POINTS_TO for the object the pointer
*    points to.
*
*
*  Misc
*  ----
*
*  void Dump_alias_mgr(const struct ALIAS_MANAGER *, const WN *, FILE *)
*    Prints out the WHIRL tree with their alias id.
*    Prints out the alias arcs.
*
*  void Print_alias_info(char *buf, const struct ALIAS_MANAGER *, WN *wn)
*    Prints out some of the alias information.
*
*  BOOL ALIAS_MANAGER::Safe_to_speculate(const WN *)
*    Returns TRUE if the object accessed by the WN node will be
*    allocated, and therefore safe to be speculatively accessed.
*    This function returns TRUE under two situations:
*    1) the WN node accesses a PREG;
*    2) the POINTS_TO of the WHIRL node satisfies the following conditions:
*       a) the POINTS_TO represents an address expr;
*       b) the base ST is valid and non-NULL;
*       c) the offset is valid and size is non-zero.
*       d) the POINTS_TO have the Safe_to_speculate attribute set
*    Condition (2c) is used to suppress the motion of array expr
*    if the size of the array is unknown.
*    Condition (2d) is used to prevent speculation of weak symbol, 
*    optional parameters, ...
*
* ========================================================================
*/

#include "defs.h"
#include "mempool.h"
#include "wn.h"

#ifdef __cplusplus
extern "C" {
#endif

struct ALIAS_MANAGER;
struct POINTS_TO;
struct PU;

typedef enum {
  NOT_ALIASED = 0,
  POSSIBLY_ALIASED = 1,
  SAME_LOCATION = 2
} ALIAS_RESULT;

/* Be careful when redefining the enum.  READ and WRITE are bit masks. */
typedef enum {
  NO_READ_NO_WRITE  = 0,
  READ              = 0x1,
  WRITE             = 0x2,
  READ_AND_WRITE    =0x3 
} READ_WRITE;

typedef UINT32 ALIAS_CONTEXT;

struct ALIAS_MANAGER *Create_Alias_Manager(MEM_POOL *, WN *);

void Delete_Alias_Manager(struct ALIAS_MANAGER *, MEM_POOL *);

void Create_Restricted_Map(MEM_POOL *);

void Copy_Restricted_Map(WN *, struct ALIAS_MANAGER *);

void Delete_Restricted_Map(void);

void Erase_Restricted_Mapping(WN *);

void Verify_Restricted_Map(const WN *, const POINTS_TO *);

BOOL Update_From_Restricted_Map(WN *, POINTS_TO *);

void Note_Invalid_Based_Symbol(const ST *);

struct ALIAS_RULE *Alias_Rule(struct ALIAS_MANAGER *);

ALIAS_CONTEXT Get_Default_Alias_Context(struct ALIAS_MANAGER *);

void Set_Alias_Context(struct ALIAS_MANAGER *, ALIAS_CONTEXT);

void Reset_Alias_Context(struct ALIAS_MANAGER *);

/* to be deleted */ void Assign_preg_alias_id(struct ALIAS_MANAGER *, WN *);

BOOL No_alias(const struct ALIAS_MANAGER *am, WN *wn);

BOOL Valid_alias(const struct ALIAS_MANAGER *am, WN *wn);

ALIAS_RESULT Aliased(const struct ALIAS_MANAGER *, WN *, WN *, BOOL ignore_loop_carried=FALSE);

ALIAS_RESULT Overlapped_base(const struct ALIAS_MANAGER *, const WN *, const WN *);

ALIAS_RESULT Aliased_with_region(const struct ALIAS_MANAGER *, const WN *, const WN *, READ_WRITE);

ALIAS_RESULT Aliased_with_intr_op(const struct ALIAS_MANAGER *, const WN *, const WN *);

void Note_Invalid_IP_Alias_Class(ALIAS_MANAGER *, const WN *);

void Invalidate_Persistent_Alias_Info(ALIAS_MANAGER *, WN *);

BOOL Homing_Load( const struct ALIAS_MANAGER *, const WN *load_wn );
void Set_Homing_Load( struct ALIAS_MANAGER *, WN *load_wn );
void Reset_Homing_Load( struct ALIAS_MANAGER *, WN *load_wn );

BOOL Homing_Store( const struct ALIAS_MANAGER *, const WN *store_wn );
void Set_Homing_Store( struct ALIAS_MANAGER *, WN *store_wn );
void Reset_Homing_Store( struct ALIAS_MANAGER *, WN *store_wn );

void Copy_alias_info(const struct ALIAS_MANAGER *, WN *, WN *);

void Duplicate_alias_info(struct ALIAS_MANAGER *, WN *, WN *);

BOOL Verify_alias(struct ALIAS_MANAGER *, WN *); /* opt_verify.cxx */

void Assign_alias_id(struct ALIAS_MANAGER *, WN *);

void Create_alias(struct ALIAS_MANAGER *, WN *);

void Create_local_alias(struct ALIAS_MANAGER *, WN *);

void Create_global_alias(struct ALIAS_MANAGER *, ST *, WN *, WN *);

void Create_formal_alias(struct ALIAS_MANAGER *, ST *, WN *, WN *);

void Create_vector_alias(struct ALIAS_MANAGER *, WN *, WN *);

void Create_unique_pointer_alias(struct ALIAS_MANAGER *, ST *, WN *, WN *);

void Create_lda_array_alias(struct ALIAS_MANAGER *, WN *, WN *);

void Print_alias_info(char *, const struct ALIAS_MANAGER *, const WN *);

BOOL May_refer_to_alloca_mem(const struct ALIAS_MANAGER *, const WN *);

BOOL Safe_to_speculate(const struct ALIAS_MANAGER *, const WN *);

void Dump_alias_mgr(const struct ALIAS_MANAGER *, const  WN *, FILE *fp);

#ifdef __cplusplus
}  /* end extern "C" */
#endif

extern void PU_adjust_addr_flags(ST*, WN*);

#endif /* opt_alias_interface.h include */

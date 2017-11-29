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


//-*-c++-*-
/* ====================================================================
* ====================================================================
*
* Module: opt_alias_rule.h
* $Revision$
* $Date$
* $Author$
* $Source$
*
* Revision history:
*  04-APR-95 lo - Split from opt_alias.h
*
* Description:
*
* ====================================================================
* ====================================================================
*/

#ifndef opt_alias_rule_INCLUDED
#define opt_alias_rule_INCLUDED	"opt_alias_rule.h"
#ifdef _KEEP_RCS_ID
static char *opt_alias_rulercs_id = 	opt_alias_rule_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */


/* ========================================================================
*
*   ALIAS RULE 
*   ----------
*   This package implements the rules used in alias analysis.
*   The set of rules used is run-time configurable by changing
*   the ALIAS CONTEXT.  The rules are managed by the ALIAS MANAGER,
*   which is the interface to LNO/CG/IPA.
*   
*   Overview
*   --------
*   The alias analysis is implemented as a rule-based system.
*   Each alias rule is implemented by a function that returns
*   TRUE or FALSE.  However, in one case, a function returns
*   NO_READ_NO_WRITE, READ, WRITE, and READ_AND_WRITE in order
*   to return better information.
*
*   The function returns FALSE if it determines that two memory 
*   operations are not aliased.  It returns TRUE if 1) the rule
*   determines that the memory operations are aliased; 2) the
*   rule cannot draw any conclusions.
*
*   The rules examine a data structure called POINTS_TO that
*   sumarize the alias information about memory operations.
*   Each memory operation have its own POINTS_TO information.
*
*   Given a set of rules, two memory operation are not aliased if
*   ANY of the rules returns FALSE.
*
*   The set of rules used for alias analysis is determined by
*   the ALIAS CONTEXT.  The context is encoded in a 64-bit integer.
*   Each rule is assigned a bit in the integer.  If the rule can
*   be used for alias analysis, the bit corresponding to its
*   position is set to 1.  The default ALIAS CONTEXT is
*   initialized in the ALIAS MANAGER constructor by examining
*   various command line options. e.g., -OPT:alias=typed ...
*
*   The ALIAS CONTEXT can also be manipulated in the midst of
*   compilation by the use Get_context()/Set_context() interface.
*   This interface is particularly useful to carry out alias
*   analysis for inlined procedures.   For example,
*   procedure A inlines procedure B.  Procedure A is written C
*   and procedure B is written in Fortran.  After inlining,
*   we still want to apply Fortran alias rules to the regions belonging
*   to B.   During inlining, IPA would call Get_context() to 
*   obtain the alias context of B and use a pragma to annotate
*   the alias context to the expanded section of B inside A.
*   During alias analysis, the default alias context (of B) is
*   used.  When the module starts processing of the inlined IR,
*   the module should read the pragma and use Set_context() to
*   reconfigure the alias rule set.  Therefore, the Fortran alias
*   rules can be applied to the inlined region.
*   
*   Similarly, if the procedure A is compiled with -OPT:alias=restrict,
*   and procedure B is compiled with -OPT:alias=typed.  Even after inlining
*   B to A, B will still be optimized with the -OPT:alias=typed.
*
*   WARNING:  The alias context is bound to the control flow structure!
*   If a memory operation is moved out of region by code motion, then
*   the alias context of the complement is used.  The implication is
*   1) if a memory operation is moved from a region of aggressive alias rules
*      to a lesser one, performance may suffer.
*   2) if a memory operation is moved from a region of less aggressive rules
*      to a more aggressive one,  incorrect results may happen.
*   Therefore, we strongly discourage inlining routines of different
*   alias levels together.
*
*   
*   Exported functions
*   ------------------
*
*   USE the interface defined here.  DO NOT CALL the alias rules 
*   directly because the implementation should change without notification.
*
*   ALIAS_CONTEXT Get_context(void)
*     returns the current alias context.
*
*   void Set_context(ALIAS_CONTEXT new_rule_set)
*     Set the current alias context to new_rule_set.
*
*   BOOL Rule_enabled(ALIAS_CONTEXT rule)
*     returns TRUE if the rule specified is allowed in the current 
*     context.
*
*
*   These following function called by the ALIAS MANAGER and the
*   computation of SSA form.  The Aliased_Memop.... function
*   examines the context and applies the enabled rules.
*
*   The rules are deliberately separated into two groups:  one that 
*   is related to user-declaration, and the other to analyses.
*   The user-declaration group is evaluated only once during SSA
*   computations.  The analyses group is evalutated twice during SSA
*   computations.
*
*   The flow is
*     1) perform context-free alias analysis
*     2) apply both declaration group and analyses group rule set.
*     3) build SSA form
*     4) perform context-sensitive alias analysis using the use-def
*        chain provided by SSA.
*     5) re-apply analyses group rules because of the extra information
*        generated by context-sensitive analysis.
*
*
*   BOOL Aliased_Memop_By_Analysis(const POINTS_TO *, const POINTS_TO *)
*     Use the rules that based on flow-sensitive POINTS_TO analysis.
*
*   BOOL Aliased_Memop_By_Declaration(cosnt POINTS_TO *, const POINTS_TO *, TY *, TY*)
*     Use the rules that based on language declarations.  
*
*   BOOL Aliased_Memop_By_Offset(cosnt POINTS_TO *, const POINTS_TO)
*     Use only the offset rule because we know the base is the same.
*
*   BOOL Aliased_Memop(const POINTS_TO *, const POINTS_TO *)
*     returns TRUE if two POINTS_TO are aliased.
*     Invoke both Aliased_Memop_By_Analysis() and Aliased_Memop_By_Declaration().
*
*   BOOL Aliased_Memop(const POINTS_TO *, const POINTS_TO *, TY *, TY *)
*     Same as Aliased_Memop(), but use the high level type information
*     passed as parameter.
*     Invoke both Aliased_Memop_By_Analysis() and Aliased_Memop_By_Declaration().
*  
*   BOOL Aliased_with_Call(ST *, const POINTS_TO *)
*     Returns TRUE if the POINTS_TO is read/modified by the procedure call.
*     
*   BOOL Aliased_with_Region( ... )
*      // to be implemented
*
*   BOOL Same_location(const WN *, const WN *, const POINTS_TO *, const POINTS_TO *)
*      Two memory operations points to exactly the same location.
*
*   
*   ALIAS RULES
*   -----------
*
*   Rules applied to all languages.
*
*   A.1: (Base rule)
*     Two memory operations are not aliased if their base is different.
*     See opt_alias_analysis.h for the definition of  Same_base() 
*     and Different_base().
*
*   A.2: (Ofst rule)
*     Two memory operations are not aliased if their base is the same,
*     and their memory ranges defined by the offset and size fields
*     does not overlap.
*
*   A.3: (Nest_rule)
*     Implementation not finished.  Supposed to deal with up-level references
*     and nested procedures ...
*
*   A.4: (Indirect rule)
*     Indirect memory access cannot access variables that are not address
*     taken and saved.   See opt_alias_analysis.h for the definition
*     of addr_taken_and_saved.
*
*   A.5: (Call_rule)
*     A procedure call does not affect local variables that are not
*     addr_taken_and_saved.
*
*   A.6: (Qual_rule)
*     Use the qualified attributes.  
*     WARNING: The Qual is physically distributed in different subroutines.
*
*     A.6.1: (pure funcition rule)
*       Pure function is not aliased to any 
*       memory operations. (See ST_pu_is_pure() in stab.h.)
*
*     A.6.2: (no side-effect function rule)
*       No side-effect does not modify the value returned by any memory
*       operations, but it might read the content of some memory.
*       (See ST_pu_no_se(st) in stab.h.)
*
*     A.6.3: (const object)
*        A const object is not modified.
*
*     A.6.4: (unique pointer)
*       If a indirect memory operation has the unique_pt attribute,
*       then the only memory operation that can alias to it is itself.
*
*
*       
*   C Rules
*
*   C.1: (ANSI Rules)
*
*     An object shall have its stored value accessed only by an lvalue that
*     has one of the following types:
*     *) the declared type of the object,
*     *) a qualified version of the declared type of the object,
*     *) a type that is signed or unsigned type corresponding to the declared type 
*        of the object,
*     *) a type that is signed or unisgined type corresponding to a 
*        qualified version of the declared type of the object,
*     *) an aggregrate or union type that includes one of the aforementioned types
*        among its members (including, recursively, a member of a subaggregate 
*        or contained union), 
*     *) a character type, or
*     *) a void type.
*
*     Use the Ragnarok interpretation here.  Objects are aliased if
*     their base types (MTYPES), after stripping off the qualifiers and
*     signed-ness, are equal.  See ANSI C 3.3 and 3.2.2.3.
*
*   C.2: (C Qualifier Rule)
*
*     C.2.1: (restricted pointer)
*       If both memory operations are restricted pointer dereference,
*       they are not aliased if their based pointer are different.
*
*
*   C++ Rules
*    Not determined.
*
*
*   Fortran Rules
*
*   F.1: (Fortran Parameter rule)
*     Fortran parameters are not aliased to anything except itself.
*
*   F.2: (Fortran Call rule)
*      Fortran actual parameters can be modified.  Fortran calls have
*      no side effect on address taken variables.
*
*   F.3: (Fortran pointer rule)
*      // not implemented
*      Fortran pointer-based variable cannot itself be a pointer.
*
*   F.4: (Fortran pointer restriction)
*      // not implemented
*      These restrictions are enforced by the frontend.
*      A pointer-based variable cannot be used as a dummy argument or in 
*      COMMON, EQUIVALENCE, DATA, or NAMELIST statements.
*      The dimension expressions for pointer-based variables must be constant
*      expressions in main programs. In subroutines and functions, the same rules
*      apply for pointer-based variables as for dummy arguments. The expression
*      can contain dummy arguments and variables in COMMON statements. Any
*      variable in the expressions must be defined with an integer value at the
*      time the subroutine or function is called.
*
*   
*   Old Rules
*
*   O.1: (Ragnarok unnamed)
*       Direct memory operations are not aliased to indirect memory operations.
*
*   O.2: (Ragnarok restricted)
*       Memory operations are not aliased if their based pointer
*       are different.
*
*
* ======================================================================== 
*/
 


class OPT_STAB;
class AUX_STAB_ENTRY;
class POINTS_TO;
class ALIAS_KIND; 
class WN;
typedef struct bs BS;
class AliasAnalyzer;

#include "optimizer.h"

//  Assign a bitpos to each rule.
//  _context in ALIAS_RULE control which rules can be applied
//
enum {
  // Rules common to all languages
  BASE_RULE = 0x1,
  OFST_RULE = 0x2,
  NEST_RULE = 0x4,
  INDR_RULE = 0x8,
  CALL_RULE = 0x10,
  QUAL_RULE = 0x20,
  ATTR_RULE = 0x40,
  CLAS_RULE = 0x80,
  IP_CLAS_RULE = 0x80000000,
  ALL_COMMON_RULES = 0x800000ff,
  DEFAULT_COMMON_RULES = 0x800000ff,

  // C Rules
  C_ANSI_RULE = 0x100,
  C_QUAL_RULE = 0x200,
  C_RESTRICT_CONST_RULE = 0x400,
  C_STRONGLY_TYPED_RULE = 0x800,
  ALL_C_RULES = 0x0f00,
  DEFAULT_C_RULES = 0x0200,

  // C++ Rules
  ALL_CXX_RULES = 0xf000,
  DEFAULT_CXX_RULES = 0xf000,

  // Fortran Rules
  F_PARM_RULE = 0x10000,
  F_CALL_RULE = 0x20000,
  F_CRAY_POINTER_RULE = 0x40000,
  ALL_F_RULES = 0x0f0000,
  DEFAULT_F_RULES = 0x020000,

  // F90 Rules
  F90_TARGET_RULE = 0x100000,
  ALL_F90_RULES = 0xf00000,
  DEFAULT_F90_RULES = 0xf00000,

  // Analysis Rules 
  FFA_RULE = 0x1000000,
  FSA_RULE = 0x2000000,
  ALIAS_ANALYZER_RULE = 0x4000000,
  ALL_ANALYSIS_RULES =  0x7000000,
  DEFAULT_ANALYSIS_RULES = 0x3000000,

  // Compatiability Rules
  IBM_DISJOINT_RULE =   0x08000000,
  RAG_RESTRICTED_RULE = 0x10000000,
  RAG_UNNAMED_RULE  =   0x20000000,
  RAG_PARMS_RULE  =     0x40000000,
  ALL_COMPATIABILITY_RULES = 0x78000000,
  DEFAULT_COMPATIABILITY_RULES = 0,
};


class ALIAS_RULE {

  ALIAS_CONTEXT _context;  // control which rules can be applied
  AliasAnalyzer *_alias_analyzer;
  
private:
    //  Obtain the basic type from TY
  INT32 Get_stripped_mtype(TY_IDX ty) const;

  // return TRUE iff
  //   o. ty1 == ty2, or 
  //   o. ty1 is of aggregate and there exist a filed <f> of ty2 
  //      where Ty1_Include_Ty2(ty1, type-of-<f>) is satisfied. 
  //
  // this is helper function of Aliased_This_Ptr_Rule ().
  BOOL Ty1_Include_Ty2 (TY_IDX ty1, TY_IDX ty2) const;

  //  This routine are used by alias analysis internally.
  //  Each function implements one of the alias rules.
  //  See doc/Mongoose/alias-analysis-design for more details.
  
  BOOL Aliased_Base_Rule(const POINTS_TO *, const POINTS_TO *) const;
  // BOOL Aliased_Ofst_Rule(const POINTS_TO *, const POINTS_TO *);  // exported
  BOOL Aliased_Static_Nest_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_Classification_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_Ip_Classification_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_F_Param_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_ANSI_Type_Rule(const POINTS_TO *, const POINTS_TO *, TY_IDX, TY_IDX) const;
  BOOL Aliased_Qualifier_Rule(const POINTS_TO *, const POINTS_TO *, TY_IDX , TY_IDX ) const;
  BOOL Aliased_Attribute_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_C_Qualifier_Rule(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_Ragnarok_Unnamed(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_Ragnarok_Restrict(const POINTS_TO *, const POINTS_TO *) const;
  BOOL Aliased_Disjoint(const POINTS_TO *, const POINTS_TO *) const;
  ALIAS_KIND Aliased_Indirect_Rule(const POINTS_TO *, const POINTS_TO *, 
                             BOOL ignore_loop_carried) const;
  BOOL Aliased_F90_Target_Rule(const POINTS_TO *, const POINTS_TO *,
			       TY_IDX , TY_IDX ) const;
  BOOL Aliased_Alias_Analyzer_Rule(const POINTS_TO *, const POINTS_TO *,bool) const;
  
public:

  ALIAS_RULE(ALIAS_CONTEXT ac, AliasAnalyzer *aa)
    : _context(ac),
      _alias_analyzer(aa)
  {}

  BOOL Aliased_Strongly_Typed_Rule(TY_IDX , TY_IDX ) const;

  //  Check if a rule is applicable
  BOOL Rule_enabled(ALIAS_CONTEXT rule) const   {  return _context & rule; }

  //  Add all call-by-reference language here!!!
  BOOL Call_by_reference(void) const   { return Rule_enabled(F_CALL_RULE); }

  // Get/Set alias context.
  void Set_context(ALIAS_CONTEXT c)    { _context = c; }
  ALIAS_CONTEXT Get_context(void)      { return _context; }

  //  Two memory operations are referencing the same location.
  BOOL Same_location(const WN *, const WN *, const POINTS_TO *, const POINTS_TO *) const;

  //  Offset,size of two memop overlapped.
  ALIAS_KIND Aliased_Ofst_Rule(const POINTS_TO *, const POINTS_TO *) const;

  //  Interface exported to the optimizer.
  //  We provide two sets of interface functions.

  //  The first set returns whether two memops are aliased.
  //
  ALIAS_KIND Aliased_Memop(const POINTS_TO *, const POINTS_TO *,
                      BOOL ignore_loop_carried = FALSE) const;
  ALIAS_KIND Aliased_Memop(const POINTS_TO *, const POINTS_TO *, TY_IDX , TY_IDX,
                      BOOL ignore_loop_carried = FALSE) const;
  ALIAS_KIND Aliased_Memop_By_Analysis(const POINTS_TO *, const POINTS_TO *,
                      BOOL ignore_loop_carried = FALSE) const;
  BOOL Aliased_Memop_By_Declaration(const POINTS_TO *, const POINTS_TO *, 
          TY_IDX ll_ty1, TY_IDX ll_ty2, 
          TY_IDX hl_ty1=(TY_IDX)0,
          TY_IDX hl_ty2=(TY_IDX)0) const;

  BOOL Aliased_with_Global(const POINTS_TO *) const;
  BOOL Aliased_with_Indirect(const POINTS_TO *) const;

  //   The second set returns whether a scalar is aliased by a CALL
  //
  READ_WRITE Aliased_with_Call(ST *, INT32, const POINTS_TO *) const;
  // READ_WRITE Aliased_with_Region(REGION *, const POINTS_TO *);

  READ_WRITE Aliased_with_Asm(const WN *, const POINTS_TO *) const;
  
  //  The third set returns a bitset representing the set of named
  //  scalars that are affected by the side effect (memop to symbol
  //  analysis). The set is a conservative estimate.
  //
  const BS *Alias_Set_Indirect(const OPT_STAB *) const;
  const BS *Alias_Set_Call_By_Value(const OPT_STAB *) const;
  const BS *Alias_Set_Call_By_Ref(const OPT_STAB *) const;
  const BS *Alias_Set_Return(const OPT_STAB *) const;
  const BS *Alias_Set_Asm(const OPT_STAB *) const;
};

typedef UINT32 LMV_ALIAS_GROUP;
inline LMV_ALIAS_GROUP Gen_LMV_alias_group(INT loop, INT grp) 
  { return (UINT(loop & 0xffff) << 16) | UINT(grp & 0xffff) ; }

#endif

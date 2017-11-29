//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_vnfre.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vnfre.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// INTERFACE DESCRIPTION:
// ----------------------
//
//    This interface exports a procedure which implements our notion of
//    redundancy elimination of totally redundant expressions based on 
//    the results of value numbering.  We term this optimization "value
//    numbering fully redundant elimination" (VNFRE::remove_redundancies).
//    After VNFRE::remove_redundancies(), all the VN methods remain valid
//    except VN::expr_stmts().
//
//    This is hooked into our SSA PRE algorithm (i.e. the ETABLE), since 
//    we reuse the last steps and many of the datastructures of that 
//    algorithm (e.g. the occurrence graph) for our algorithm.  Note that
//    this file does not just export the remove_redundancies() procedure,
//    although that represents our top-level algorithm, but it also must
//    export call-back functions to be used by the ETABLE while one
//    invocation of remove_redundancies() is on the call-stack.  We export
//    both the top-level algorithm (remove_redundancies) and the call-back
//    functions under a name-space VNFRE.
//
//    The memory pools used are the same as for an etable.  The algorithm is
//    invoked as follows:
//
//        VNFRE::remove_redundancies(gvn, etable, cu);
//
//    The call-back functions are as follows, where the aim is to maintain
//    a valid cr-->valnum mapping, but where we will not maintain a valid 
//    cr-->stmt_list mapping:
//
//        VNFRE::get_valnum(cr) : Returns an ordinal value number 
//           for the given coderep.  If the coderep does not have a
//           valid value number (its Coderep_Id() is outside our range
//           or is zero), then we need to compute the value number using
//           the information gathered in the global value numbering object
//           (gvn).  Note that a computed value number will always be an
//           existing value number, or, if none is found, Zero (0).
//
//        VNFRE::add_valnum(cr, valnum) : Unconditionally maps the 
//           given coderep to the given value number in the "gvn" passed 
//           in to "VNFRE::remove_redundancies".  The cr must have a non-zero
//           Coderep_Id() and the valnum must be a valid one.  This call may
//           cause "gvn" maps to grow, since it may introduce new coderep ids.
//           We update any relevant maps maintained as part of VNFRE algorithm,
//           including the move of all cr occurrences from the occurrence
//           list of its previous value number (if valid) to the occurrence
//           list for its new value number (if it is yet to be processed).
//
//        VNFRE::replace_occurs(old_cr, new_cr, stmt) : Replaces
//           all occurrences of the form <old_cr, stmt> with <new_cr, stmt>,
//           assuming the two codereps have identical value numbers.
//           This is not done recursively (so call it for all levels of
//           rehashing).  Redundant if we ignore codereps in EXP_OCCURS for
//           VNFRE, and instead record a value number with occurrences (TODO).
//
//        VNFRE::move_rhs_occurs(old_stmt, new_stmt) : The rhs of old_stmt
//           has been moved to become the rhs of new_stmt.  We update 
//           occurrence lists accordingly.
//
//        VNFRE::new_occurs(new_stmt) : Recursively traverses all expressions
//           in the "new_stmt", and inserts all relevant occurrences into 
//           worklists yet to be processed by redundancy elimination.
//
//        VNFRE::delete_occurs(stmt, cr) : Recursively traverses all 
//           expressions in cr, and for any subexpressions "subcr" yet to 
//           be processed by redundancy elimination, all <stmt, subcr>
//           occurrences will be removed from our occurrence lists.
//
//
// SEE ALSO:
// ---------
//
//    opt_eocc.cxx: Functionality related to occurrence nodes in worklist.
//    opt_etable.h: SSAPRE and associated algorithms.
//    opt_eavail.h: SSAPRE propagation of availability flags.
//    opt_cse.cxx:  Last steps of SSAPRE algorithm.
//    opt_htable.h: Rehashing and coderep data-structures.
//    opt_cfg.h:    Control flow graph.
//    opt_ssa.h:    Control flow graph in SSA form.
//    opt_vn.h :    Implementation of global value numbering.
//
// ====================================================================
// ====================================================================


#ifndef opt_vnfre_INCLUDED
#define opt_vnfre_INCLUDED "opt_vnfre.h"

class ETABLE;
class VN;
class COMP_UNIT;
class CODEREP;
class STMTREP;
class EXP_OCCURS;
class EXP_WORKLST;

namespace VNFRE 
{
   // Algorithm for value numbering redundancy elimination.
   //
   void remove_redundancies(VN        &vn, 
			    ETABLE    &etable, 
			    COMP_UNIT *comp_unit);

   // Call-back routines during processing of remove_full_redundancies().
   //
   UINT32       get_valnum(const CODEREP *cr);

   void         add_valnum(const CODEREP *cr, UINT32 valnum);

   void         replace_occurs(const CODEREP *old_cr, 
			       CODEREP       *new_cr, 
			       const STMTREP *stmt);

   void         move_rhs_occurs(const STMTREP *old_stmt, 
				STMTREP       *new_stmt);

   void         new_occurs(STMTREP *new_stmt);

   void         delete_occurs(const EXP_OCCURS *occur, 
			      const CODEREP    *within_cr);

   EXP_WORKLST *get_worklst(const CODEREP *cr);

} // Close namespace VNFRE


#endif // opt_vnfre_INCLUDED

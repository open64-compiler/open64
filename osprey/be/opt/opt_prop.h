//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_prop.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_prop.h,v $
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
// ====================================================================


#ifndef opt_prop_INCLUDED
#define opt_prop_INCLUDED	"opt_prop.h"

#ifdef _KEEP_RCS_ID
static char *opt_proprcs_id = 	opt_prop_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

// forward declaration
class CFG;
class BB_NODE;
class OPT_STAB;
class CODEMAP;
class CODEREP;

// when deciding if it is possible to propagate through phi in phi 
// simplification, the two expressions on either side can be of different 
// syntactical forms; this tells which form to use when generating the common
// form after the phi 
enum PROP_THRU_PHI_PREFERENCE {
    EITHER_SIDE,
    LEFT_SIDE,
    RIGHT_SIDE,
};

class COPYPROP {
private:
  OPT_STAB   *_opt_stab;        // the optimizer symtab
  CFG        *_cfg;             // the control flow graph
  CODEMAP    *_htable;          // the hash table
  MEM_POOL   *_loc_pool;        // the permenant pool for new nodes
  BOOL        _disabled;        // disabled copy prop currently
  DYN_ARRAY<CODEREP *> _visited_vec; // array of nodes visited by Propagatable
  DYN_ARRAY<CODEREP *> _rehashed_vec; // array of nodes processed and rehashed 
				      // by Copy_propagate_cr
  DYN_ARRAY<CODEREP *> _rehashed_to_vec; // node rehashed to from corresponding
					 // node in _rehashed_vec
  BOOL	      _past_ret_reg_def;// true if currently working in a region between
				// a def of a dedicated return preg and RETURN

  OPT_STAB *Opt_stab(void)const { return _opt_stab; }
  CFG      *Cfg(void) const     { return _cfg; }
  CODEMAP  *Htable(void) const 	{ return _htable; }
  MEM_POOL *Loc_pool(void)const { return _loc_pool; }

  void      Add_visited_node(CODEREP *x) { _visited_vec[_visited_vec.Newidx()] = x; }
  void	    Add_rehashed_node(CODEREP *x, CODEREP *y) {
			    _rehashed_vec[_rehashed_vec.Newidx()] = x;
			    _rehashed_to_vec[_rehashed_to_vec.Newidx()] = y; }

  COPYPROP(void);               // REQUIRED UNDEFINED UNWANTED methods
  COPYPROP(const COPYPROP&);    // REQUIRED UNDEFINED UNWANTED methods
  COPYPROP& operator = (const COPYPROP&); // REQUIRED UNDEFINED UNWANTED methods

  PROPAGATABILITY Propagatable(CODEREP *x, BOOL chk_inverse,
			       AUX_ID propagating_var, BOOL icopy_phase,
			       BOOL inside_cse, INT32 *height,INT32*weight,
			       BOOL in_array, BB_NODE *curbb );
  CODEREP *Copy_propagate_cr(CODEREP *x, BB_NODE *curbb, 
			     BOOL inside_cse, BOOL in_array, BOOL no_complex_preg = FALSE);
  void     Copy_propagate_stmt(STMTREP *stmt, BB_NODE *bb); 
  BOOL	   Var_has_as_value_on_the_other_path(CODEREP *var, CODEREP *value,
					      BOOL var_on_left, BB_NODE *bb);
  BOOL	   Propagatable_thru_phis(CODEREP *lexp, CODEREP *rexp, BB_NODE *bb,
				  CODEREP *phi_simp_var,
				  PROP_THRU_PHI_PREFERENCE *pref);
  CODEREP *Rehash_thru_phis(CODEREP *cr, BB_NODE *bb);
  CODEREP *Strictly_identical_phi_opnd(PHI_NODE *phi, BB_NODE *bb);
  CODEREP *Identical_phi_opnd(PHI_NODE *phi, BB_NODE *bb);
  INT32 Invertible_occurrences(CODEREP *var, CODEREP *cr);
  BOOL Is_function_of_itself(STMTREP *stmt, OPT_STAB *sym);
  BOOL Is_function_of_cur(CODEREP *var, CODEREP *cur_var);
  CODEREP *Form_inverse(CODEREP *v, CODEREP *x, CODEREP *forming_x);
  CODEREP *Rehash_inverted_expr(CODEREP *x, BOOL icopy_phase);
  BB_NODE *Propagated_to_loop_branch(BB_NODE *srcbb, BB_NODE *destbb);
  CODEREP *Get_node_rehashed_to(CODEREP *x);
  CODEREP *Prop_const_init_scalar(CODEREP *x, AUX_ID var_aux_id);
  void     Fix_identity_assignment(STMTREP *);

public:
  COPYPROP( CODEMAP *htable, OPT_STAB *opt_stab, CFG *cfg, MEM_POOL *lpool):
    _opt_stab(opt_stab), _htable(htable), _cfg(cfg), _loc_pool(lpool), 
    _visited_vec(lpool), _rehashed_vec(lpool), _rehashed_to_vec(lpool)
    { _disabled = FALSE; _past_ret_reg_def = FALSE;
    }
  ~COPYPROP(void) { _visited_vec.Free_array(); 
		    _rehashed_vec.Free_array();
		    _rehashed_to_vec.Free_array(); }

  void      Unvisit_nodes(void);
  CODEREP *Prop_var(CODEREP *x, BB_NODE *curbb, BOOL icopy_phase, 
		    BOOL inside_cse, BOOL in_array, BOOL no_complex_preg = FALSE);
  CODEREP *Prop_ivar(CODEREP *x, BB_NODE *curbb, BOOL icopy_phase, 
		     BOOL inside_cse, BOOL in_array, BOOL no_complex_preg = FALSE);
  void Copy_propagate(BB_NODE *bb);

  BOOL Disabled(void) const { return _disabled; }
  void Set_disabled(void)   { _disabled = TRUE; }
  void Reset_disabled(void) { _disabled = FALSE; }
  BOOL Past_ret_reg_def(void) const { return _past_ret_reg_def; }
  void Set_past_ret_reg_def(void) { _past_ret_reg_def = TRUE; }
  void Reset_past_ret_reg_def(void) { _past_ret_reg_def = FALSE; }
}; // end of class COPYPROP

#endif  // opt_prop_INCLUDED

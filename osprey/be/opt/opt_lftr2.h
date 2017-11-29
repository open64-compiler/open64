//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_lftr2.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_lftr2.h,v $
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


#ifndef opt_lftr2_INCLUDED
#define opt_lftr2_INCLUDED      "opt_lftr2.h"
#ifdef _KEEP_RCS_ID
static char *opt_lftr2rcs_id =  opt_lftr2_INCLUDED"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "opt_defs.h"
#include "opt_base.h"
#include "cxx_memory.h"
#include "opt_bb.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_etable.h"		// ETABLE class
#ifndef opt_util_INCLUDED
#include "opt_util.h"
#endif

//----------------------------------------------------------------------------

// information needed to maintain the comparisons dealing with a lftr_var
class LFTR_VAR : public SLIST_NODE {
private:
  AUX_ID	 _aux_id;	// aux_id for this lftr_var
  EXP_OCCURS    *_comp_list;	// list of EXP_OCCURS, comparisons
  INT32		 _list_size;	// length of EXP_OCCURS list

  LFTR_VAR(void);
  LFTR_VAR(const LFTR_VAR&);
  LFTR_VAR& operator = (const LFTR_VAR&);

public:
  LFTR_VAR(AUX_ID lftr_var)
    {  _aux_id = lftr_var;
       _comp_list = NULL;
       _list_size = 0;
    }
  ~LFTR_VAR(void)	{ }
  void Init(void)	{ }

  DECLARE_SLIST_NODE_CLASS( LFTR_VAR )

  // there is an Add_comparison in the LFTR class also
  // adds given COMP occurrence in DPO
  void Add_comparison(EXP_OCCURS *);

  // makes a new COMP occurrence and adds it in DPO
  void Add_comparison(CODEREP *, STMTREP *, INT32, MEM_POOL *, BOOL);

  // find the appropriate LFTR_VAR node in the list (that matches lftr_var)
  LFTR_VAR *Find_comp_list(AUX_ID);

  EXP_OCCURS *Occ_list(void) const	{ return _comp_list; }
  void	      Set_occ_list(EXP_OCCURS *occ) { _comp_list = occ; }
  AUX_ID      Lftr_var(void) const	{ return _aux_id; }
  INT32	      Size(void) const		{ return _list_size; }
  void	      Dec_list_size(void)	{ _list_size--; }
  void	      Inc_list_size(void)	{ _list_size++; }
  void	      Print(FILE *);
};

class LFTR_VAR_CONTAINER : public SLIST {
private:
  LFTR_VAR_CONTAINER(const LFTR_VAR_CONTAINER&);
  LFTR_VAR_CONTAINER& operator = (const LFTR_VAR_CONTAINER&);

  DECLARE_SLIST_CLASS(LFTR_VAR_CONTAINER, LFTR_VAR)
public:  
  ~LFTR_VAR_CONTAINER(void)             { }
  void            Init(void)		{ }
};

class LFTR_VAR_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(LFTR_VAR_ITER, LFTR_VAR, LFTR_VAR_CONTAINER)
public:
  void            Init(void)               { }
};

//----------------------------------------------------------------------------

// main LFTR class that controls all the lists and memory pools
class LFTR {
private:
  BOOL		 _trace;	// trace flag for LFTR
  BOOL		 _lftr_on;	// same as WOPT_Enable_LFTR
  INT32		 _num_substitutions; // statistic on number of substitutions
  MEM_POOL	 _mem_pool;	// LFTR mem pool
  ETABLE	*_etable;	// for ETABLE::Append_real_occurrence()
  CODEMAP	*_htable;	// for HTABLE::Canon_base()
  CFG		*_cfg;		// for CFG::Find_innermost_loop_contains()
  EXP_WORKLST_ITER2 *_exp_iter;	// iter for list of worklsts

  LFTR_VAR     **_hash_vec;	// hash table of comparison lists
  INT32		 _hash_size;	// size of hash table
  INT32		 _len;		// size of current lftr_var list being used

  DYN_ARRAY<INT32> _stmt_no;	// array to map stmt ids to stmt numbers
  INT32		 _last_stmt;	// last statement number

  LFTR(void);
  LFTR(const LFTR&);
  LFTR& operator = (const LFTR&);

  // cache the size for a particular lftr_list
  void Set_len(INT32 size)	{ _len = size; }

  // check if its one of the expressions LFTR recognizes
  // can't check if it is inside a loop because EXP_WORKLST doesn't have stmt
  BOOL Is_lftr_exp(CODEREP *cr)
    {	if (Lftr_on()) {
          switch(OPCODE_operator(cr->Op())) {
		case OPR_ADD: // PPP add more cases later
		case OPR_SUB:
		case OPR_MPY:
#ifdef TARG_X8664
		case OPR_CVT:
#endif
      			return TRUE;
    	  }
	}
	return FALSE;
    }

  CODEREP *Set_lhs_and_rhs(CODEREP *cr, CODEREP **rhs)
    { *rhs = (cr->Kid_count() == 2) ? cr->Opnd(1) : NULL;
      return cr->Opnd(0);
    }

  // add a comparison to appropriate list
  void Add_comparison(CODEREP *cr, AUX_ID lftr_var, STMTREP *stmt,
		      INT32 stmt_kid_num);

  // remove a comparison from a specific list
  void Remove_comparison(EXP_OCCURS *comp, AUX_ID lftr_var);

  // hash table routines
  void Alloc_hash_vec(INT32);		// called by constructor
  IDX_32 Hash_lftr_var(AUX_ID aux_id)
    { Is_True(aux_id != ILLEGAL_AUX_ID,
	      ("LFTR::Hash_lftr_var, aux_id out of range"));
      return aux_id % _hash_size;
    }
  LFTR_VAR *Find_comp_list(AUX_ID lftr_var); // main entry point for hash fn.

  // canonicalize a CR expression and find the LFTR var, -1 if none found
  CODEREP *Find_lftr_var(CODEREP *);

  // a different version of it, look for the CODEREP in 'exp' that has
  // the same Aux_id as the second parameter 'var'
  CODEREP *Find_lftr_var(CODEREP *exp, CODEREP *var);

  AUX_ID Find_lftr_var_id(CODEREP *cr) // wrapper to return aux_id
    { CODEREP *ret = Find_lftr_var(cr);
      return (ret == NULL) ? ILLEGAL_AUX_ID : ret->Aux_id();
    }

  // replace an LFTR var in an expression with a substitute expression
  // recursive
  CODEREP *Replace_lftr_var(CODEREP *, AUX_ID, CODEREP *);

  // find the tempcr associated with a SR expression
  CODEREP *Find_SR_tempcr(STMTREP *, CODEREP *);

  void Lftr_comparison(CODEREP *cr, STMTREP *stmt, INT32 stmt_kid_num);

  void Check_for_obsolete_comparison(EXP_OCCURS *);

  BOOL Can_only_increase(CODEREP *, AUX_ID);

  void Replace_use(const BB_LOOP * loop, CODEREP * cr_old, CODEREP * cr_new);
  void Replace_use(CODEREP*, CODEREP *, CODEREP *);

  // print an occurrence and a string
  void Print_occ(FILE *fp, const char *str, EXP_OCCURS *occ)
    { fprintf(fp,"%s\n",str);
      occ->Print(fp);
      occ->Occurrence()->Print(0,fp);
    }

public:
  // pushes LFTR memory, allocates hash table
  LFTR(ETABLE *, CODEMAP *, CFG *, mUINT32);
  // pops LFTR memory
  ~LFTR(void);				

  BOOL Trace(void)			{ return _trace; }
  BOOL Lftr_on(void)			{ return _lftr_on; }
  INT32 Num_substitutions(void)		{ return _num_substitutions; }
  ETABLE *Etable(void)			{ return _etable; }
  CODEMAP *Htable(void)			{ return _htable; }
  CFG *Cfg(void)			{ return _cfg; }
  IDX_32 Stmt_no(IDX_32 index)		{ return _stmt_no[index]; }
  void Free_hash_vec(void);		// called by ETABLE::Init_worklst()

  // list of worklsts iterator
  EXP_WORKLST_ITER2 *Exp_iter(void) const	 { return _exp_iter; }
  void Set_exp_iter(EXP_WORKLST_ITER2 *exp_iter) { if (Lftr_on())
						     _exp_iter = exp_iter; }

  // check if its one of the comparisons LFTR recognizes
  BOOL Is_comparison(CODEREP *cr)
    {	if (cr->Kind() == CK_OP) {
          switch(OPCODE_operator(cr->Op())) {
	        case OPR_LNOT:
		case OPR_EQ:
		case OPR_NE:
		case OPR_GT:
		case OPR_GE:
		case OPR_LT:
		case OPR_LE:
      			return TRUE;
	  }
	}
	return FALSE;
    }

  // called from ETABLE::Per_worklst_cleanup, clear all Def_occur()
  void Clear_def_occurs(EXP_WORKLST *exp_worklst);

  // Called from Step 1 & 6, find suitable LFTR comparisons, screen out others
  void Insert_comp_occurrence(CODEREP *cr, STMTREP *stmt, INT32 stmt_kid_num)
    {
      if (Lftr_on() && Is_comparison(cr))
        Lftr_comparison(cr, stmt, stmt_kid_num);
    }

  // Assign statement numbers to each statement
  void Assign_stmt_no(STMTREP *);

  // hash an expression from worklist
  EXP_OCCURS_PAIR *Exp_hash(EXP_WORKLST *worklst);
  // return size of the comparison list for this expression
  INT32 Len(void)			{ return _len; }

  // Step 6, replace LFTR comparisons
  void Replace_comparison(EXP_OCCURS *comp, BOOL cur_expr_is_sr_candidate);

  void Remove_lftr_non_candidates(void); // called at end of step 1

  // if the real comparison is replaced with a tempcr, need to remove the
  // corresponding comp occur
  void Remove_comp_occur(EXP_OCCURS *occur);
  // find the corresponding comp occur and update the stmt pointer
  void Update_comp_occur_stmt(EXP_OCCURS *occur, STMTREP *stmt);

  void Print(FILE *);			// dump all LFTR data structures
};

#endif // opt_lftr_INCLUDED

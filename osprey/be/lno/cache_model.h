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


/**
***
*** Description:
***
***	The cache model, which chooses the best tiling and block size, given
***	which loops we may place innermost and which we can tile.
***
*** Reserved Prefix:
***
***	None, but this reserves the right to use the SNL prefix, since it's
***	really modelling SNLs.  E.g. SNL_MODEL, if anything.
***
*** CACHE_MODEL_INFO:
***
***	Description:
***
***	    The return value of Cache_Model.
***
***	Fields:
***
***	    double	Hidable_Cycles_Per_Iter;
***	    double	Non_Hidable_Cycles_Per_Iter;
***
***		Depending on the cache model, the latency of certain
***		references are hidable.  Return cycles per iter of latency
***		that are hidable and not.
***
*** Exported functions
***
***	void Cache_Model(
***		ARRAY_REF*	arl,
***		INT		depth,
***		INT		required_inner,
***		const INT*	inners,
***		INT		ninners,
***		const INT*	tiles,
***		INT		ntiles,
***		const INT*	unrolls,
***             const DOLOOP_STACK* stack,
***             BOOL            blocking_disabled,
***             const INT*      required_blocksize,
***             double          model_cost,
***             INT             num_refs,
***
***		-- return values:
***
***		INT*		new_order,
***             INT*          nstrips,        // return val
***             INT*            stripdepth,     // return val
***             INT*            iloop,          // return array
***             INT*            stripsz,        // return array
***             INT*            striplevel,     // return array
***             double*         hidable_cycles_per_iter,
***             double*         non_hidable_cycles_per_iter,
***	        double*		doverhead_best,
***             double*         thidable_cycles_per_iter,
***             double*         tnon_hidable_cycles_per_iter,
***	        double*		tdoverhead_best
***	);
***
***	     arl:	From the model.
***	     depth:	The depth of the innermost loop.  Outermost is zero.
***			So the number of loops in the nest is depth+1.
***	     required_inner: Which loop will be innermost, because other
***			parts of the compiler are modelling this case.
***	     inners:	Which loops in [0..depth] may be placed inner.
***			Presumably, depth is one of them.
***	     ninners:	How many legal inner loops there are.
***			Presumably, at least one.
***	     tiles:	Which loops in [0..depth] may participate in the
***			tile, but cannot be innermost.  No loops from here
***			will go into a tile unless all loops from
***			legal_inners go into a tile.  This is how we avoid
***			imperfect loop interchange -- loops outside of
***			imperfect code go in legal_tiles but not legal_inners.
***	     ntiles:	How many legal inner loops there are.  May be zero.
***	     unrolls:	For each loop in [0..depth], how much that loop has
***			been unrolled.
***          stack:     the do loop stack.
***          blocking_disabled: don't block.  Just evaluate this required inner.
***          required_blocksize[loopno*MHD_MAX_LEVESL+mhd_level]:
***                     If this value is >= 0, then pragmas request that this
***                     loop at this level have this blocksize (0=in the block,
***                     but not stripped).  This suggestion is taken lightly.
***          model_cost: how much the model thinks the loop would cost
***                      without cache effects.
***          num_refs:  the number of different references in the
***                     innermost loop.  E.g. do i a(i,j),a(i+1,j), a(i,j+1)
***                     has two, because the first two CSE in the i loop.
***
***	     new_order:	The permutation to apply.  e.g. new_order[0] tells
***			which loop the new outer loop comes from.
***          nstrips:   *nstrips will hold the number of strips produced
***                     by tiling.
***          stripdepth: *stripdepth will hold the depth of the loop
***                     (after reordering) where the strips will be placed.
***          iloop:     iloop[s] with hold the depth (after reordering)
***                     of the s'th stripped loop.
***          stripsz:   stripsz[s] with hold the blocksize
***                     of the s'th stripped loop.
***          striplevel:striplevel[s] with hold the cache/memory hierarcy
***                     level this is stripped for, plus 1 (mhd_level 0 is
***                     striplevel 1).
***
***          cycles_per_iter: how many cycles per iteration for
***                     memory and overhead.
***	     do_overhead: cycles per iter for overhead only.  Therefore,
***                     cycles_per_iter - do_overhead is memory cost.
**/

/** $Revision: 1.5 $
*** $Date: 04/12/21 14:57:12-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.cache_model.h $
**/

#ifndef cache_model_INCLUDED
#define cache_model_INCLUDED "cache_model.h"


#ifdef _KEEP_RCS_ID
static char *cache_model_rcs_id = cache_model_INCLUDED "$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif

class ARRAY_REF;
// class DOLOOP_STACK;

/**
*** Class FORMULA
***
***     Description:
***
***         Builds up a tree expressing an arithmetic formula.  The arithmetic
***         is double precision floating point.  There are numbered variables,
***         constants, and the binary operators +, -, *, /.  The function
***         Eval() is passed the value of each variable, and a double value is
***         returned.
***
***         FORMULA_VAR returns the value of the given variable, the index
***         into the passed in array.  But also, temporary values can be
***         stored into registers and reused later in the same eval.  For
***         example, to compute partial result, store it into scratch
***         register 0 (there are dozens of scratch registers) and use it
***         twice later, one would generate code like
***                ((r0=...),(... r0 ... r0)
***         where the assignment is created with FORMULA::Set(0, tree)
***         and that is the left child of a comma operator and the
***         values of r0 are obtained with FORMULA::Use(0).
***
***     Exported Types:
***
***         FORMULA_OP
***
***             an enumeration, with values
***               FORMULA_ADD
***               FORMULA_SUB
***               FORMULA_MUL
***               FORMULA_DIV
***               FORMULA_MAX
***               FORMULA_MIN
***               FORMULA_GE
***               FORMULA_GT
***               FORMULA_LE
***               FORMULA_LT
***               FORMULA_AND         return x&&y, short circuit
***               FORMULA_OR          return x||y, short circuit
***               FORMULA_COND
***               FORMULA_FCONST
***               FORMULA_VAR
***               FORMULA_SET
***               FORMULA_USE
***               FORMULA_COMMA
***
***     Exported Functions:
***
***         FORMULA::FORMULA(BOOL TRUE, double constant)
***
***             Create a formula consisting of a constant value
***
***         FORMULA::FORMULA(BOOL FALSE, int variable)
***
***             Create a formula consisting of a single variable.
***
***         FORMULA::FORMULA(FORMULA_OP fop, FORMULA* left, FORMULA* right)
***
***             Create a formula consisting of the fop (+,-,*,/) and the
***             two operands.
***
***         double FORMULA::Eval(INT nvars, const double* vars) const
***         double FORMULA::Eval(INT nvars, const mINT64* vars) const
***         double FORMULA::Eval(INT nvars, const mINT32* vars) const
***
***             Compute the value of the function.  An array of variable
***             values is passed in.  E.g. vars[0] contains the value of
***             of the 0th variable.  The INT* version is supplied for
***             convenience -- the values are cast to doubles before they
***             are used.  vars may be NULL, in which case nvars must
***             be zero.
***
***         void FORMULA::Print(FILE* f) const
***
***             Print in infix notation, so easy to read.
***
***     Creating a formula:
***
***         Start with "FORMULA::Fpool = &LNO_local_pool;" or likewise
***         so we know from where to allocate nodes.  Then call the static
***         constructor-like formulas
***
***             FORMULA* FORMULA::Var(INT* k)
***                 return a formula that will evaluate to the
***                 k-th user variable.
***             FORMULA* FORMULA::Const(double d)
***                 a formula that will evaluate to the constant d.
***             FORMULA* FORMULA::Set(INT r, FORMULA* f)
***                 a formula that will set scratch register r to have the
***                 value of f, and return that value.
***             FORMULA* FORMULA::Set0(INT r, FORMULA* f)
***                 same as Set(), but f == NULL is okay, in which case use
***                 a constant value of zero.
***             FORMULA* FORMULA::Use(INT r, FORMULA* f)
***                 a formula that will return the value of scratch register r
***             FORMULA* FORMULA::{Add,Sub,Mul,Div,Comma,Ge,Gt,Le,Lt,Max,Min)
***                                         (FORMULA* left, FORMULA* right)
***                 a formula that returns left op right.
***             FORMULA* FORMULA::Cond(FORMULA* cond, FORMULA* l, FORMULA* r)
***                 a formula that returns cond!=0?l:r, where only one
***                 side is evaluated.
**/

enum FORMULA_OP {
  FORMULA_BAD = 666,

  FORMULA_ADD = 1001,   // to keep random stuff from working
  FORMULA_SUB,
  FORMULA_MUL,
  FORMULA_DIV,
  FORMULA_MAX,
  FORMULA_MIN,
  FORMULA_GE,
  FORMULA_GT,
  FORMULA_LE,
  FORMULA_LT,
  FORMULA_AND,
  FORMULA_OR,
  FORMULA_COND,         // execute LHS if Cond != 0, RHS otherwise
  FORMULA_FCONST,
  FORMULA_VAR,          // read from 'vars' parameter
  FORMULA_SET,          // set _scratch register
  FORMULA_USE,          // use _scratch register
  FORMULA_COMMA         // left child probably a set
};

// Set Fpool, the pool to use.  Call New to get new elements.
// Elements are freed by freeing the pool -- there is no need for a
// destructor.

// Note that you can preset valued in the array v, but you can also
// have the formula itself set a value with Set.

class FORMULA {

 public:

  void                  Print(FILE* f, FORMULA_OP parent) const;
  enum                  {SCRATCH_REGISTERS = 10*LNO_MAX_DO_LOOP_DEPTH};


  double                Eval(INT nvars, const double* vars) const;
  double                Eval(INT nvars, const mINT64* vars) const;
  double                Eval(INT nvars, const mINT32* vars) const;

  FORMULA*              Add_To_Variable(INT var, INT val);

  FORMULA*              Duplicate() const;
  void                  Print(FILE* f) const {Print(f, FORMULA_BAD);}

  static MEM_POOL* Fpool;
  inline static FORMULA* Var(INT v) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(v, FALSE), Fpool);
  }
  inline static FORMULA* Use(INT v) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(v, TRUE), Fpool);
  }
  inline static FORMULA* Set(INT v, FORMULA* f) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(v, f), Fpool);
  }
  inline static FORMULA* Set0(INT v, FORMULA* f) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return Set(v,f?f:Const(0.0));
  }
  inline static FORMULA* Const(double v) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(v), Fpool);
  }
  inline static FORMULA* Cond(FORMULA* cond, FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(cond, left, right), Fpool);
  }
  inline static FORMULA* Comma(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_COMMA, left, right), Fpool);
  }
  inline static FORMULA* Comma3(FORMULA* c1, FORMULA* c2, FORMULA* c3) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return Comma(c1, Comma(c2, c3));
  }
  inline static FORMULA* Comma4(FORMULA* c1, FORMULA* c2,
                                FORMULA* c3, FORMULA* c4) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return Comma(c1, Comma3(c2, c3, c4));
  }
  inline static FORMULA* Comma5(FORMULA* c1, FORMULA* c2,
                                FORMULA* c3, FORMULA* c4, FORMULA* c5) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return Comma(c1, Comma4(c2, c3, c4, c5));
  }
  inline static FORMULA* Add(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_ADD, left, right), Fpool);
  }
  inline static FORMULA* Sub(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_SUB, left, right), Fpool);
  }
  inline static FORMULA* Mul(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_MUL, left, right), Fpool);
  }
  inline static FORMULA* Div(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_DIV, left, right), Fpool);
  }
  inline static FORMULA* Max(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_MAX, left, right), Fpool);
  }
  inline static FORMULA* Min(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_MIN, left, right), Fpool);
  }
  inline static FORMULA* Ge(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_GE, left, right), Fpool);
  }
  inline static FORMULA* Gt(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_GT, left, right), Fpool);
  }
  inline static FORMULA* Le(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_LE, left, right), Fpool);
  }
  inline static FORMULA* Lt(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_LT, left, right), Fpool);
  }
  inline static FORMULA* And(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_OR, left, right), Fpool);
  }
  inline static FORMULA* Or(FORMULA* left, FORMULA* right) {
    FmtAssert(Fpool, ("Fpool uninitialized"));
    return CXX_NEW(FORMULA(FORMULA_OR, left, right), Fpool);
  }

  // debugging aid
  ~FORMULA() {
    _fop = FORMULA_BAD;
  }

 private:

  FORMULA(double v) {
    _fop = FORMULA_FCONST;
    _fconst = v;
  }
  FORMULA(INT v, BOOL use_reg) {
    _fop = use_reg ? FORMULA_USE : FORMULA_VAR;
    _var = v;
  }
  FORMULA(INT v, FORMULA* f) {
    _fop = FORMULA_SET;
    _var = v;
    _kids.Left = f;
  }
  FORMULA(FORMULA_OP fop, FORMULA* left, FORMULA* right) {
    Is_True(left && right, ("Missing child for FORMULA::FORMULA()"));
    Is_True(fop == FORMULA_ADD || fop == FORMULA_SUB ||
            fop == FORMULA_MUL || fop == FORMULA_DIV ||
            fop == FORMULA_MAX || fop == FORMULA_MIN ||
            fop == FORMULA_GE || fop == FORMULA_GT ||
            fop == FORMULA_LE || fop == FORMULA_LT ||
            fop == FORMULA_AND || fop == FORMULA_OR ||
            fop == FORMULA_COMMA,
            ("Bad call to formula constructor: fop=%d", fop));
    _fop = fop;
    _kids.Left = left;
    _kids.Right = right;
  }
  FORMULA(FORMULA* cond, FORMULA* left, FORMULA* right) {
    Is_True(left && right && cond, ("Missing child for FORMULA::FORMULA()"));
    _fop = FORMULA_COND;
    _kids.Cond = cond;
    _kids.Left = left;
    _kids.Right = right;
  }

  double                Eval(const double* vars) const;
  double                Eval_Inlined(const double* vars) const {
    if (_fop == FORMULA_FCONST)
      return _fconst;
    else if (_fop == FORMULA_VAR)
      return vars[_var];
    else
      return Eval(vars);
  }

  FORMULA_OP    _fop;
  union {
    double      _fconst;
    INT         _var;
  };   // wasting some space, since Cond and Right could go in here too.
  struct {
    FORMULA*          Cond;
    FORMULA*          Left;
    FORMULA*          Right;
  }             _kids;

  static double _scratch[SCRATCH_REGISTERS];
};

class COMPUTE_FOOTPRINT_RVAL {
  FORMULA*      _formula;          // sum of Wformula and Rformula
 public:
  FORMULA*      RFormula;          // formula for reads
  FORMULA*      WFormula;          // formula for writes
  FORMULA*      AllFormula();      // total ... use only after
                                   // RFormula and Wformula are final
  INT           D;
  COMPUTE_FOOTPRINT_RVAL() :
        WFormula(NULL), RFormula(NULL), _formula(NULL), D(0) {}
  void          Print(FILE* f) const;
};

extern void Set_Cache_Model_Statics(INT mhd_level); 

extern COMPUTE_FOOTPRINT_RVAL Compute_Footprint(
                    const ARRAY_REF*  arl,
                    INT               nloops,   // to be tiled
                    const INT*        loops,    // to be tiled
                    const INT*        arl_stripsz,
                    const INT64*      est_iters,
                    const INT64*      max_iters,
                    const INT*        unrolls,
                    INT               depth,
                    INT               stripdepth,
                    const INT*        permute_order,
                    INT               v_first,
                    INT64             outersz,
                    BOOL              using_tlb,
                    INT               middle_loop_no);

void Cache_Model(ARRAY_REF* arl,
                 INT depth,
                 INT required_inner,
                 const INT* legal_inners,
                 INT nlegal_inners,
                 const INT* legal_tiles,
                 INT nlegal_tiles,
                 const INT* unrolls,
                 const DOLOOP_STACK* stack,
		 BOOL blocking_disabled,
		 const INT *required_blocksize,
                 double model_cost,
		 INT num_refs,

                 INT* new_order,                       // array
                 INT* nstrips,                         // return val
                 INT* stripdepth,                      // return val
                 INT* iloop,                           // return array
                 INT* stripsz,                         // return array
                 INT* striplevel,                      // return array
                 double* cycles_per_iter,              // scalar
		 double* doverhead_best);	       // scalar
#endif

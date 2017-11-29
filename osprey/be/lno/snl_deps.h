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


// -*-C++-*-

/**
*** Description:
***
***	Information about an snl to be gathered after we have found one.
***	The main information is simply the dependences.  It's most convenient
***	to summarize the dependences within an snl simply as a list of
***	all the lexicographically positive dependences, without duplicates.
***	Since we don't apply fission or fusion, we don't need more.  Also,
***	the dependences are slightly different from those in the graph.  We
***	look in the graph for all dependences and copy them into the
***	data structures here, but for non-perfectly nested statements, we
***	recompute dependences pretending they were in the deepest nest.
***	(Note we could get more precision still by indicating to the
***	dependence analyzer whether the imperfect code is below or above
***	the nest, and therefore including the lower or upper bounds of
***	loops further in, as appropriate, to the dependence analyzer.  But
***	it's not a big deal.)
***
***	Once all the data is gathered, you can query the dependence summary
***	as to whether the nest is fully permutable.  If not, you may ask it
***	if any skew will make it so.  You can also ask whether any
***	transformation is legal.  NOTE: In my thesis, I advocated a SRP
***	transformation.  That is unnecessarily general, given realistic
***	loop nests.  Both the skew and permutation code can work on subnests,
***	so that we can cheaply explore alternatives when the entire nest is
***	neither untransformable nor (transformable to be) fully permutable.
***
***	TODO OK: There may be too many dependences here, depending on whether
***	we plan to do distribution.  It would be a very good idea to recognize
***	that we will distribute out some imperfect code if necessary, and
***	therefore not include those dependences.
***
*** Reserved Prefix:
***
***	SNL, just like for the other snl_ files.
***
*** Exported Values:
***
***	None.
***
*** Exported Types:
***
***	SNL_DEP
***
***	    A dep that has arithmetic defined on it.  A simple modification
***	    of DEP.  One could even imagine integrating it with DEP.
***	    But it's slightly different, in that it implements
***	    "3 or more", "-5 or less", etc.
***
***	    Distance
***		A field holding that constant
***	    Moreless
***		an enumerated type of 
***			SNL_DEP_PLUS
***			SNL_DEP_MINUS
***			SNL_DEP_EXACT
***			SNL_DEP_STAR
***
***	    SNL_DEP(DEP)
***
***		Create a SNL_DEP from a dep
***
***	    SNL_DEP()
***
***		Create a SNL_DEP with value "*"
***
***	    DEP Dep() const
***
***		The value of a SNL_DEP.
***
***	    friend SNL_DEP operator * (INT, const SNL_DEP&)
***	    friend SNL_DEP operator + (const SNL_DEP&, const SNL_DEP&)
***
***		SNL_DEP arithmetic.
***
***	    void Print(FILE*) const;
***
***		The value of a SNL_DEP
***
***	    INT Min() const
***	    INT Max() const
***
***		Return the smallest (largest) possible value.
***		Had better not be unbounded in that direction.
***
***	    BOOL Unbounded_Min() const
***	    BOOL Unbounded_Max() const
***
***		Negatively (positively) unbounded.
***
***	    SNL_DEP Negate() const
***	    void Negate_Me()
***
***		Reverse dep
***
***	SNL_DEP_MATRIX
***
***	    SNL_DEP_MATRIX(const SNL_DEP_INFO&, MEM_POOL*);
***	    SNL_DEP_MATRIX(const SNL_DEP_MATRIX*, MEM_POOL*);
***	    ~SNL_DEP_MATRIX();
***
***		Construct and destruct.
***
***	    SNL_DEP	operator ()(INT d, INT i) const
***	    SNL_DEP&	operator ()(INT d, INT i)
***
***		Access an element.
***
***	    void Print(FILE*) const;
***
***		Print
***
***	    BOOL Is_Legal() const
***
***		Lex non-neg.
***
***	    void Apply(const IMAT& u, INT first);
***	    void Apply(const INT* permutation);
***
***		Transform this as per the matrix u.  u may be smaller than
***		depth x depth.  First indicates the first dimension of the
***		dependences that this applies to -- u is assumed to be the
***		identity for the first 'first' dimensions and any dimensions
***		after Nloops() - first - u.Rows().  The second form applies
***		the permutation, which must be of size depth.
***
***	    INT Ndep() const
***	    INT Nloops() const
***	    MEM_POOL* Pool() const
***
***		Number of dependences, depth, pool this was initialized to.
***
***	SNL_DEP_INFO
***
***	    Summary of dependences within an SNL.
***
***	    SNL_DEP_INFO(INT firstc, INT depth, INT num_unused_dim,
***			 const DOLOOP_STACK& stk, MEM_POOL* pool)
***
***		firstc: set First_Component()
***		depth:  set Nloops()
***		num_unused_dim: how many loops are "bad" outside this nest
***		stk: the loop stack all the way through to the inner loop
***		     of the snl
***		pool: set Pool()
***
***	    ~SNL_DEP_INFO()
***
***		Destroy one of these.
***
***	    BOOL All_Stars() const
***
***		Is the nest untransformable because of dependences?
***
***	    void Set_All_Stars(BOOL v)
***
***		Set whether the nest is untransformable (rarely used).
***
***	    const DEPV_LIST& Dv_List() const
***
***		The list of dependences that summarizes all dependences
***		in the nest.
***
***	    INT Nloops() const
***
***		How many loops are in the nest.
***
***	    INT First_Component() const
***
***		Which dependence component in the graph is the leading
***		component in Dv_List.  For example, if we have an SNL
***		consisting of loops k and l, with
***
***		    DO i
***		     DO j
***		      DO k
***		       DO l
***
***		and num_unused_dim is 1 (the i loop is bad), then dependences
***		in the graph will have 3 components, but only the last two
***		need be remembered for this nest.  First_Component() is 1.
***
***	    MEM_POOL* Pool() const
***
***		Where to allocate temporary data structures.
***		Do not push or pop this pool through this's lifetime.
***
***	    const DOLOOP_STACK& Stack1() const
***	    const DOLOOP_STACK&	Stack2() const
***
***		Two copies of the passed in stk, typically used for
***		calls to dependence calculation routines, which need these.
***
***	    void Enter(const DEPV*, INT, BOOL);
***	    void Enter(const DEPV_ARRAY*, BOOL);
***
***		Add a dependence to the nest.  It's careful about avoiding
***		duplicates. If BOOL is true, don't count (+,*,...,*) as all
***	        stars.  Useful for parallelism analysis. 
***
***	    void Print(FILE* f) const
***
***		Print one of these babies.
***
***	    BOOL Is_Fully_Permutable(INT from, INT to) const
***	    BOOL Is_Fully_Permutable() const
***
***		Is the specified subnest fully permutable?  The second
***		version	checks the entire nest.
***
***	    IMAT* U_Fully_Permutable(INT from, INT to, MEM_POOL*mp) const
***	    IMAT* U_Fully_Permutable(MEM_POOL*mp) const
***
***
***		Suggests a skew/reversal that will make the specified subnest
***		fully permutable.  The second version checks the entire nest.
***
***         const SNL_DEP_INFO_BAD_DEPS& Bad_Deps() const
***
***             Only set if LNO_Analysis is set.  SNL_DEP_INFO_BAD_DEPS is
***             an adjustable array of struct {EINDEX16 e; mINT16 loop;}.
***             It's a list of bad dependence edges.  e=0 means generic
***             dependence problem (e.g. a missing vertex).  loop=0 means
***             that because of this problem, the outermost loop of the
***             next cannot be transformed.  loop=Nloops()-1 means all the
***             loops cannot be transformed.
***
***	SNL_ANAL_INFO
***
***	    Summary of analysis information within an SNL.
***
***	    SNL_ANAL_INFO(
***		const SNL_FIND_NEST_INFO*,
***		BOOL gtransform,
***		MEM_POOL*
***	    );
***
***		Build the SNL_DEP_INFO for this nest, and a hash table
***		of WN* to lex information, so that later we can go through
***		the WN*'s quickly and have that information ready.  The
***		pool is from where we allocate temporary storage for this
***		node.  The 'gtransform' variable indicates whether we should
***		gather this data assuming we will do a general transformation
***		or an invariant.  The general transform includes dependences
***             from the non-perfect code (if that isn't distributable) and
***             so is strictly more pessimistic than without gtransform.
***             So, if dependences are computed with gtransform, and then
***             it's desired to apply an invariant transform, the dependences
***             computed here are conservative, so no problems reusing the
***             dependences here (so long as the loop is transformable to
***             that depth).
***
***	    ~SNL_ANAL_INFO() {}
***
***		Destroy one of these
***
***	    const SNL_DEP_INFO& Deps()
***
***		The dependences for this nest.
***
***	    MEM_POOL* Pool() const
***
***		Values of passed in parameters.
***
***	    void Enter_Lex(WN* ldst, INT lex);
***	    Find_Lex(WN* ldst) const
***
***		Interfaces to a hash table that stores wn*'s and their
***		associated lexical ordering.  If Find_Lex(wn) returns 0,
***		then wn isn't in the map.  (So Enter_Lex should not be
***		called with lex=0.)
***
*** 	    void Print(FILE* f) const
***
***		Print one of these puppies.
***
***	    void Enter_Deps(WN* wn, INT lex)
***
***		Enter the dependences with wn as a source, given the lex
***		value.
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef snl_deps_INCLUDED
#define snl_deps_INCLUDED "snl_deps.h"

#ifdef _KEEP_RCS_ID
static char *snl_deps_rcs_id = snl_deps_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

class ARRAY_DIRECTED_GRAPH16;
#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif

class SNL_DEP {
 public:
  INT Distance;
  enum {SNL_DEP_PLUS, SNL_DEP_MINUS, SNL_DEP_EXACT, SNL_DEP_STAR} Moreless;
  SNL_DEP(DEP d);
  SNL_DEP() {Distance = 0; Moreless = SNL_DEP_EXACT;}
  DEP Dep() const;
  friend SNL_DEP operator * (INT a, const SNL_DEP& d);
  friend SNL_DEP operator + (const SNL_DEP& dd, const SNL_DEP& d);
  void operator += (const SNL_DEP& d);
  void Print(FILE* f) const;
  BOOL Unbounded_Max() const {
    return Moreless == SNL_DEP_STAR || Moreless == SNL_DEP_PLUS;
  }
  BOOL Unbounded_Min() const {
    return Moreless == SNL_DEP_STAR || Moreless == SNL_DEP_MINUS;
  }
  BOOL Max() const {
    Is_True(!Unbounded_Max(), ("Bad call to SNL_DEP::Max()"));
    return Distance;
  }
  BOOL Min() const {
    Is_True(!Unbounded_Min(), ("Bad call to SNL_DEP::Min()"));
    return Distance;
  }
  void Negate_Me() {
    Distance = -Distance;
    if (Moreless == SNL_DEP_PLUS)
      Moreless = SNL_DEP_MINUS;
    else if (Moreless == SNL_DEP_MINUS)
      Moreless = SNL_DEP_PLUS;
  }
  SNL_DEP Negate() const {
    SNL_DEP newd = *this;
    newd.Negate_Me();
    return newd;
  }
};

class SNL_DEP_INFO;

class SNL_DEP_MATRIX {

 public:

  SNL_DEP_MATRIX(const SNL_DEP_INFO&, MEM_POOL*);
  SNL_DEP_MATRIX(const SNL_DEP_MATRIX*, MEM_POOL*);
  ~SNL_DEP_MATRIX();

  SNL_DEP	operator ()(INT d, INT i) const {
    Is_True(d >= 0 && d < _ndep, ("Depenendence number out of range"));
    Is_True(i >= 0 && i < _nloops, ("Depth number out of range"));
    return _deps[d*_nloops + i];
  }
  SNL_DEP&	operator ()(INT d, INT i) {
    Is_True(d >= 0 && d < _ndep, ("Depenendence number out of range"));
    Is_True(i >= 0 && i < _nloops, ("Depth number out of range"));
    return _deps[d*_nloops + i];
  }
  void		Print(FILE*) const;
  BOOL		Is_Legal() const;
  BOOL		Is_Fully_Permutable(INT from, INT to) const;
  BOOL		Is_Fully_Permutable() const
    { return Is_Fully_Permutable(0, Nloops()-1); }
  void		Apply(const IMAT& u, INT first);
  void		Apply(const INT* permutation);

  INT		Ndep() const {return _ndep;}
  INT		Nloops() const {return _nloops;}
  MEM_POOL*	Pool() const {return _pool;}

 private:

  SNL_DEP*	_deps;
  INT		_ndep;
  INT		_nloops;
  MEM_POOL*	_pool;
};

struct SNL_DEP_INFO_BAD_DEP_ENTRY {
  EINDEX16    e;                // the edge making the bad dep
                                // edge 0 means missing dependence information
  mINT16      loop;             // the loop it screws up: 0 = outermost in nest
                                // nloops-1 is innermost
};
typedef DYN_ARRAY<SNL_DEP_INFO_BAD_DEP_ENTRY> SNL_DEP_INFO_BAD_DEPS;

extern void Print(FILE*f, const SNL_DEP_INFO_BAD_DEPS&);

class SNL_DEP_INFO {

  friend class SNL_ANAL_INFO;

 public:

  const DEPV_LIST&	Dv_List() const {return _dv_list;}
  BOOL			All_Stars() const {return _all_stars;}
  INT			Nloops() const {return _nloops;}
  INT			First_Component() const {return _first_component;}
  MEM_POOL*		Pool() const {return _pool;}
  const DOLOOP_STACK&	Stack1() const {return _stack1;}
  const DOLOOP_STACK&	Stack2() const {return _stack2;}

  SNL_DEP_INFO(INT firstc, INT nloops, INT num_unused_dim,
	       const DOLOOP_STACK& stk, MEM_POOL* pool);
  ~SNL_DEP_INFO() {}

  void		Enter(const DEPV*, INT, EINDEX16 edge, BOOL parallel=FALSE);
  void		Enter(const DEPV_ARRAY*, EINDEX16 edge, BOOL parallel=FALSE);
  void		Print(FILE* f) const;
  void		Set_All_Stars(BOOL v) {_all_stars = v;}
  IMAT*		U_Fully_Permutable(INT from, INT to, MEM_POOL*mp) const;
  IMAT*		U_Fully_Permutable(MEM_POOL*mp) const
    { return U_Fully_Permutable(0, Nloops()-1, mp); }
  const SNL_DEP_INFO_BAD_DEPS& Bad_Deps() const {return _bad_deps;}

 private:

  DEPV_LIST		_dv_list;
  BOOL			_all_stars;
  INT			_nloops;
  INT			_first_component;
  MEM_POOL*		_pool;
  DOLOOP_STACK		_stack1;
  DOLOOP_STACK		_stack2;
  SNL_DEP_INFO_BAD_DEPS _bad_deps;

  SNL_DEP_INFO();		// undefined

};

struct LEX_DEPTH {
  INT	Lex;
  INT	Depth;
  LEX_DEPTH(INT l, INT d) : Lex(l), Depth(d) {}
  LEX_DEPTH(INT l) : Lex(l), Depth(-1)	// so works with hash table
    {Is_True(l == 0, ("Bad use of LEX_DEPTH(INT) constructor"));}
  LEX_DEPTH() : Lex(0), Depth(-1) {}
  LEX_DEPTH(const LEX_DEPTH& ld) : Lex(ld.Lex), Depth(ld.Depth) {}
};

struct SNL_ANAL_INFO {

  // at loop of depth 'loop', does the lower bounds depend on index the
  // depth 'index' or not?  Conservatively, yes (not constant).
  // If depth >= 64, then we are conservative.

  struct CONST_BOUNDS_INFO {
    CONST_BOUNDS_INFO(const DOLOOP_STACK*);
    BOOL	Lbconst(INT loop, INT index) const {
      Is_True(index < loop, ("Bad call to Lbconst()"));
      return loop<64 && index<64 ? (_lbconst[loop]&(INT64(1)<<index)) != 0 : 0;
    }
    BOOL	Ubconst(INT loop, INT index) const {
      Is_True(index < loop, ("Bad call to Ubconst()"));
      return loop<64 && index<64 ? (_ubconst[loop]&(INT64(1)<<index)) != 0 : 0;
    }
    void	Print(FILE* f) const;

   private:

    mINT64		_lbconst[64]; // overflow of 64 not possible.  So okay.
    mINT64		_ubconst[64]; // overflow of 64 not possible.  So okay.
  };

  const SNL_DEP_INFO&	Body_Deps() const {return _body_deps;}
  const SNL_DEP_INFO&	Imperfect_Deps() const {return _imperfect_deps;}
  MEM_POOL*		Pool() const {return _pool;}

  SNL_ANAL_INFO(const SNL_NEST_INFO*,
		BOOL gtransform,
                ARRAY_DIRECTED_GRAPH16*,
		MEM_POOL*);
  ~SNL_ANAL_INFO();

  LEX_DEPTH	Find_Lex(WN* ldst) const
    { return _lexinfo.Find(ldst); }
  void		Print(FILE* f) const;

  BOOL		Above_Main_Nest(INT lex) const
    { return lex <= _lex_last_above_innermost; }
  BOOL		Below_Main_Nest(INT lex) const
    { return lex >= _lex_first_below_innermost; }

  BOOL          Above_Is_Distributable() const
    { return _above_is_distributable;}
  BOOL          Below_Is_Distributable() const
    { return _below_is_distributable;}

 private:

  SNL_ANAL_INFO();		// undefined

  void		Enter_Lex(WN* ldst, LEX_DEPTH ld) {
    _lexinfo.Enter(ldst, ld);
  }
  void		Enter_Deps(WN*, LEX_DEPTH);

  enum		{HT_ELTS=247};	// not a power of two, when pointer is elt
  HASH_TABLE<WN*,LEX_DEPTH>	_lexinfo;
  INT				_lex_last_above_innermost;
  INT				_lex_first_below_innermost;
  SNL_DEP_INFO			_body_deps;
  SNL_DEP_INFO			_imperfect_deps;
  MEM_POOL*			_pool;
  BOOL				_above_is_distributable;
  BOOL				_below_is_distributable;
  WN*				_inner_loop;
  INT				_depth_inner;
  const CONST_BOUNDS_INFO	_ci;
};

#endif

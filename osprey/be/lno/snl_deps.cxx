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


// -*-C++-*-

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_utils_CXX      "snl_utils.cxx"
const static char *rcs_id =   snl_utils_CXX "$Revision: 1.5 $";

#include <sys/types.h>
#include "snl.h"
#include "lwn_util.h"
#include "dep_graph.h"

//--------------------------------------------------------------------
// Utility
//--------------------------------------------------------------------

void Print(FILE* f, const SNL_DEP_INFO_BAD_DEPS& bad_deps)
{
  for (INT i = bad_deps.Lastidx(); i >= 0; i--) {
    fprintf(f, "<e=%d,loop=%d>", bad_deps[i].e, bad_deps[i].loop);
  }
}

//--------------------------------------------------------------------
// SNL_DEP_MATRIX: Convenient representation of dependences
//--------------------------------------------------------------------

SNL_DEP operator * (INT a, const SNL_DEP& d)
{
  SNL_DEP dd;
  dd.Distance = a * d.Distance;
  if (a == 0 || d.Moreless == SNL_DEP::SNL_DEP_EXACT)
    dd.Moreless = SNL_DEP::SNL_DEP_EXACT;
  else if (a > 0)
    dd.Moreless = d.Moreless;
  else {
    switch (d.Moreless) {
     case SNL_DEP::SNL_DEP_PLUS:
      dd.Moreless = SNL_DEP::SNL_DEP_MINUS;
      break;
     case SNL_DEP::SNL_DEP_MINUS:
      dd.Moreless = SNL_DEP::SNL_DEP_PLUS;
      break;
     case SNL_DEP::SNL_DEP_STAR:
      dd.Moreless = SNL_DEP::SNL_DEP_STAR;
      break;
     default:
      FmtAssert(0, ("Impossible"));
    }
  }
  return dd;
}

SNL_DEP operator + (const SNL_DEP& d, const SNL_DEP& dd)
{
  SNL_DEP ddd;
  ddd.Distance = d.Distance + dd.Distance;
  if (d.Moreless == SNL_DEP::SNL_DEP_EXACT || d.Moreless == dd.Moreless)
    ddd.Moreless = dd.Moreless;
  else if (dd.Moreless == SNL_DEP::SNL_DEP_EXACT)
    ddd.Moreless = d.Moreless;
  else
    ddd.Moreless = SNL_DEP::SNL_DEP_STAR;
  return ddd;
}

void SNL_DEP::operator += (const SNL_DEP& d)
{
  Distance += d.Distance;
  if (Moreless == SNL_DEP::SNL_DEP_EXACT || Moreless == d.Moreless)
    Moreless = d.Moreless;
  else if (d.Moreless == SNL_DEP::SNL_DEP_EXACT)
    ;
  else
    Moreless = SNL_DEP::SNL_DEP_STAR;
}

DEP SNL_DEP::Dep() const
{
  switch (Moreless) {
   case SNL_DEP_EXACT:
    return DEP_SetDistance(Distance);
   case SNL_DEP_PLUS:
    if (Distance == 0)
      return DEP_SetDirection(DIR_POSEQ);
    else if (Distance > 0)
      return DEP_SetDirection(DIR_POS);
   case SNL_DEP_MINUS:
    if (Distance == 0)
      return DEP_SetDirection(DIR_NEGEQ);
    else if (Distance < 0)
      return DEP_SetDirection(DIR_NEG);
  }
  return DEP_SetDirection(DIR_STAR);
}

void SNL_DEP::Print(FILE* f) const
{
  switch (Moreless) {
   case SNL_DEP_PLUS:
    fprintf(f, "%d+", Distance);
    break;
   case SNL_DEP_MINUS:
    fprintf(f, "%d-", Distance);
    break;
   case SNL_DEP_EXACT:
    fprintf(f, "%d", Distance);
    break;
   case SNL_DEP_STAR:
    fprintf(f, "*");
    break;
  }
}

SNL_DEP::SNL_DEP(DEP d)
{
  if (DEP_IsDistance(d)) {
    Distance = DEP_Distance(d); Moreless = SNL_DEP_EXACT;
  }
  else
    switch (DEP_Direction(d)) {
     case DIR_POS:
      Distance = 1; Moreless = SNL_DEP_PLUS; break;
     case DIR_NEG:
      Distance = -1; Moreless = SNL_DEP_MINUS; break;
     case DIR_POSEQ:
      Distance = 0; Moreless = SNL_DEP_PLUS; break;
     case DIR_NEGEQ:
      Distance = 0; Moreless = SNL_DEP_MINUS; break;
     case DIR_EQ:
      Is_True(0, ("Impossible"));
     default:
      Distance = 0; Moreless = SNL_DEP_STAR; break;
    }
}

//--------------------------------------------------------------------
// SNL_DEP_MATRIX: Convenient representation of dependences
//--------------------------------------------------------------------

SNL_DEP_MATRIX::SNL_DEP_MATRIX(const SNL_DEP_INFO& info, MEM_POOL* pool)
: _pool(pool),
  _ndep(info.Dv_List().Len()),
  _nloops(info.Nloops()),
  _deps(CXX_NEW_ARRAY(SNL_DEP, info.Dv_List().Len()*info.Nloops(), pool))
{
  DEPV_CONST_ITER dit(&info.Dv_List());
  const DEPV_NODE* n;
  INT d = 0;
  for (n = dit.First(); !dit.Is_Empty(); n = dit.Next(), d++) {
    for (INT i = 0; i < _nloops; i++)
      (*this)(d,i) = SNL_DEP(DEPV_Dep(n->Depv,i));
  }
}

SNL_DEP_MATRIX::SNL_DEP_MATRIX(const SNL_DEP_MATRIX* m, MEM_POOL* pool)
: _pool(pool),
  _ndep(m->_ndep),
  _nloops(m->_nloops),
  _deps(CXX_NEW_ARRAY(SNL_DEP, m->_ndep*m->_nloops, pool))
{
  for (INT d = 0; d < _ndep; d++)
    for (INT i = 0; i < _nloops; i++)
      (*this)(d,i) = (*m)(d,i);
}

void SNL_DEP_MATRIX::Print(FILE* f) const
{
  for (INT d = 0; d < _ndep; d++) {
    fprintf(f, "[");
    for (INT i = 0; i < _nloops; i++) {
      (*this)(d,i).Print(f);
      fprintf(f, "%s", i == _nloops - 1 ? "]" : " ");
    }
  }
  fprintf(f, "\n");
}

BOOL SNL_DEP_MATRIX::Is_Legal() const
{
  for (INT d = 0; d < _ndep; d++) {
    for (INT i = 0; i < _nloops; i++) {
      SNL_DEP dep = (*this)(d,i);
      if (dep.Unbounded_Min() || dep.Min() < 0)
	return FALSE;
      else if (dep.Min() > 0)
	break;
    }
  }
  return TRUE;
}

void SNL_DEP_MATRIX::Apply(const IMAT& u, INT first)
{
  FmtAssert(u.Rows() == u.Cols(), ("Bad u for Apply()"));
  FmtAssert(first + u.Rows() <= _nloops, ("Bad first for Apply()"));
  for (INT d = 0; d < Ndep(); d++) {
    SNL_DEP newd[SNL_MAX_LOOPS];	// init to zero
    INT i;
    for (i = 0; i < u.Rows(); i++) {
      for (INT j = 0; j < u.Rows(); j++)
	newd[i] += u(i,j) * (*this)(d,j+first);
    }
    for (i = 0 ; i < u.Rows(); i++)
      (*this)(d,i+first) = newd[i];
  }
}

void SNL_DEP_MATRIX::Apply(const INT* permutation)
{
  FmtAssert(_pool != &SNL_local_pool, ("Pool problem"));

  MEM_POOL_Push_Freeze(&SNL_local_pool);
  {
    SNL_DEP_MATRIX tmp(this, &SNL_local_pool);
    for (INT d = 0; d < Ndep(); d++) {
      for (INT i = 0; i < Nloops(); i++)
        (*this)(d,i) = tmp(d,permutation[i]);
    }
  }
  MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
}



SNL_DEP_MATRIX::~SNL_DEP_MATRIX()
{
  CXX_DELETE_ARRAY(_deps, _pool);
}

BOOL SNL_DEP_MATRIX::Is_Fully_Permutable(INT from, INT to) const
{
  if (!Is_Legal())
    return FALSE;

  for (INT d = 0; d < Ndep(); d++) {
    INT i;
    for (i = 0; i < from; i++) {
      if ((*this)(d,i).Min() >= 1)
	break;
    }

    if (i != from)
      continue;

    for ( ; i <= to; i++) {
      if ((*this)(d,i).Unbounded_Min() || (*this)(d,i).Min() < 0)
	return FALSE;
    }
  }

  return TRUE;
}

//--------------------------------------------------------------------
// SNL_DEP_INFO: Dependence representation for a loop nest
//--------------------------------------------------------------------

SNL_DEP_INFO::SNL_DEP_INFO(INT firstc, INT nloops, INT num_unused_dim,
			   const DOLOOP_STACK& stk, MEM_POOL* pool)
: _first_component(firstc), _all_stars(FALSE), _nloops(nloops), _pool(pool),
  _dv_list(nloops, num_unused_dim, pool), _stack1(pool), _stack2(pool),
  _bad_deps(pool)
{
  for (INT i = 0; i < nloops + firstc + num_unused_dim; i++) {
    _stack1.Push(stk.Bottom_nth(i));
    _stack2.Push(stk.Bottom_nth(i));
  }
}

void SNL_DEP_INFO::Print(FILE* f) const
{
  if (All_Stars())
    fprintf(f, "<all stars>\n");
  else {
    _dv_list.Print(f);
    fprintf(f, "\n");
  }
  fprintf(f, "bad_deps:");
  ::Print(f, _bad_deps);
  fprintf(f, "\n");
}

void SNL_DEP_INFO::Enter(const DEPV_ARRAY* dv, EINDEX16 edge, BOOL parallel)
{
  for (INT i = 0; i < dv->Num_Vec(); i++)
    Enter(dv->Depv(i), dv->Num_Dim(), edge, parallel);
}

void SNL_DEP_INFO::Enter(const DEPV* dv, INT components, EINDEX16 edge, 
  BOOL parallel)
{
  FmtAssert(components >= Nloops() + First_Component(),
	    ("Too short dependence vector for SNL_DEP_INFO::Enter()"));

  // short cut, if don't need the information

  if (!LNO_Analysis) {
    if (All_Stars())
      return;
  }

  // could all the components all be zero (first that couldn't be zero
  // must be +).

  INT d;
  for (d = 0; d < First_Component(); d++) {
    if (DEP_Direction(DEPV_Dep(dv,d)) == DIR_POS)
      return;
  }

  // Is this an all stars dependence?  If so, note that.  If all zeros,
  // just ignore it.

  switch (DEP_Direction(DEPV_Dep(dv,First_Component()))) {
   case DIR_POS:
    if (!parallel) { 
      for (d = 1; d < Nloops(); d++) {
	if (DEP_Direction(DEPV_Dep(dv,First_Component()+d)) != DIR_STAR)
	  break;
      }
      if (d == Nloops()) {
	_all_stars = TRUE;
	INT entry = _bad_deps.Newidx();
	_bad_deps[entry].e = edge;
	_bad_deps[entry].loop = Nloops() - 1;
	return;
      }
    } 
    break;
   case DIR_EQ:
    for (d = 1; d < Nloops(); d++) {
      if (DEP_Direction(DEPV_Dep(dv,First_Component()+d)) != DIR_EQ)
	break;
    }
    if (d == Nloops())		// all zeros, so ignore it
      return;
    break;
  }

  // The farthest component in that has a negative component to it, and
  // therefore inhibits SNL transformation.  Of course, some transformations
  // remove some of these problems.

  if (LNO_Analysis) {
    INT negloop = -1;
    for (d = 0; d < Nloops(); d++) {
      if (DEP_Direction(DEPV_Dep(dv,First_Component()+d)) & DIR_NEG)
        negloop = d;
    }
    if (negloop >= 0) {
      INT entry = _bad_deps.Newidx();
      _bad_deps[entry].e = edge;
      _bad_deps[entry].loop = negloop;
    }
  }

  // now get out of here if don't want to insert

  if (All_Stars())
    return;

  // Insert.  Keep it simple: if identical, toss away, otherwise keep

  DEPV_ITER iter(&_dv_list);
  for (DEPV_NODE *n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
    INT d;
    for (d = 0; d < Nloops(); d++) {
      DEP d1 = DEPV_Dep(n->Depv,d);
      DEP d2 = DEPV_Dep(dv,First_Component()+d);
      if (DEP_IsDistance(d1)) {
	if (DEP_IsDistance(d2) && DEP_Distance(d1) == DEP_Distance(d2))
	  continue;
      }
      else {
	if (!DEP_IsDistance(d2) && DEP_Direction(d1) == DEP_Direction(d2))
	  continue;
      }
      break;
    }
    if (d == Nloops())
      return;
  }

  DEPV* newdv = DEPV_Create(Pool(), Nloops());
  for (d = 0; d < Nloops(); d++)
    DEPV_Dep(newdv,d) = DEPV_Dep(dv,d+First_Component());

  _dv_list.Append(CXX_NEW(DEPV_NODE(newdv), Pool()));
}

IMAT* SNL_DEP_INFO::U_Fully_Permutable(INT from, INT to, MEM_POOL* pool) const
{
  FmtAssert(pool != &SNL_local_pool, ("Pool problem"));

  if (All_Stars())
    return NULL;

  MEM_POOL_Push_Freeze(&SNL_local_pool);

  // step 0: transformation to make fully permutable (skew and reversal
  // matrix) goes in 'm'.

  IMAT* m = CXX_NEW(IMAT(to-from+1, to-from+1, pool), pool);
  m->D_Identity();

  // step 1: put data in matrix format, which is easier to deal with.
  // Discard lexicographically positive [in 0 ... from-1] vectors.

  INT veccnt = Dv_List().Len();
  INT dcnt = to-from+1;
  INT* per_vector_info = CXX_NEW_ARRAY(INT, veccnt, &SNL_local_pool);

  DEPV_CONST_ITER dit(&Dv_List());
  INT v = 0;
  INT vmax = 0;
  const DEPV_NODE* n = 0;
  for (n = dit.First(); !dit.Is_Empty(); n = dit.Next()) {
    INT d;
    for (d = 0; d < from; d++) {
      DEP d1 = DEPV_Dep(n->Depv,d);
      if (DEP_Direction(d1) == DIR_POS)
	break;
    }

    per_vector_info[v++] = (d == from);		// if FALSE, lexpos
    if (d == from)
      vmax++;
  }

  SNL_DEP* deps = CXX_NEW_ARRAY(SNL_DEP, vmax * dcnt, &SNL_local_pool);
#define DP(i,j) (deps[(i) + (j)*(vmax)])

  v = 0;
  INT vv = 0;
  for (n = dit.First(); !dit.Is_Empty(); n = dit.Next()) {
    if (per_vector_info[v++]) {
      for (INT d = from; d <= to; d++)
	DP(vv,d-from) = SNL_DEP(DEPV_Dep(n->Depv,d));
      vv++;
    }
  }

  Is_True(vv == vmax, ("Bug in U_Fully_Permute"));

  // step 2.  Try to make the DP components all positive.

  INT d;
  for (d = 0; d < dcnt; d++) {
    INT unbounded_max = 0;
    INT unbounded_min = 0;
    INT minneg = 0;
    INT maxpos = 0;

    INT v;
    for (v = 0; v < vmax; v++) {
      SNL_DEP dep = DP(v,d);
      if (dep.Unbounded_Max())
	unbounded_max = 1;
      else if (dep.Max() > maxpos)
	maxpos = dep.Max();
      if (dep.Unbounded_Min())
	unbounded_min = 1;
      else if (dep.Min() < minneg)
	minneg = dep.Min();
    }

    if (unbounded_max && unbounded_min)
      break;

    if (unbounded_min) {

      // no choice, must reverse.  First reverse dependences

      for (v = 0; v < vmax; v++)
	DP(v,d).Negate_Me();

      // Now update matrix to indicate reversal

#ifdef Is_True_On
      for (INT x = 0; x < dcnt; x++)
	  Is_True((*m)(d,x) == (x==d), ("Confusion with matrix"));
#endif
      (*m)(d,d) = -1;

      // Now update our friendly little variables

      unbounded_min = 0;
      unbounded_max = 1;

      INT tmp = minneg;
      minneg = -maxpos;
      maxpos = -tmp;
    }

    // if nothing wrong, keep going

    if (minneg == 0)
      continue;

    // So there's a skew we need to perform.
    // This is guaranteed to work, except that we choose
    // to disallow large skewing factors.  If the skew is larger
    // than three, then there's a significant chance we are skewing
    // past some ugly bounds.  TODO OK: this is a heuristic/hack that
    // may not be justifiable.  Fix as you will.

    // First see where there's no choice about the skews.  If a vector
    // has only one strictly positive component in 0 to d-1, and if d
    // is a negative component, then d must be skewed with respect to
    // that one loop.  So do that skew.

    // per_vector_info indicates which vectors are fixed by the proposed skews.
        
    INT factors[SNL_MAX_LOOPS];
    INT dd;
    for(dd = 0; dd < d; dd++)
      factors[dd] = 0;

    for (v = 0; v < vmax; v++) {
      INT mindist = DP(v,d).Min();
      if (mindist >= 0)
	continue;
            
      // Now look at the leading dependence components.  Since this vector
      // is lexicographically positive, at least one is positive.  If exactly
      // one, that's where there's no choice.
            
      INT ddpos = -1;
      INT ddposmin = 0;

      per_vector_info[v] = 1;		// taken care of
      for (dd = 0; dd < d; dd++) {
	INT ddmin = DP(v,dd).Min();
	FmtAssert(ddmin >= 0, ("Make fully permutable bug"));
	if(ddmin > 0) {
	  if(ddpos == -1) {
	    ddpos = dd;
	    ddposmin = ddmin;
	  }
	  else {
	    ddpos = -2;
	    per_vector_info[v] = 0;	// not taken care of
	  }
	}
      }

      FmtAssert(ddpos != -1, ("Make fully permutable bug2"));
      if (ddpos != -2) {
	INT newfactor = (-mindist + ddposmin - 1)/ddposmin;
	if (newfactor > factors[ddpos])
	  factors[ddpos] = newfactor;
      }
    }

    // The per_vector_info is 0 if still not taken care of.  Take
    // care of it in some gross fashion -- it doesn't really matter
    // how.  We'll just use the innermost choice, which tends to be
    // less disruptive.

    for (v = 0; v < vmax; v++) {
      if (per_vector_info[v])
	continue;

      INT vdist = DP(v,d).Min();
      Is_True(vdist < 0, ("I'm confused"));
      INT ddpos = -1;
      INT ddposmin;
      for (INT dd = 0; dd < d; dd++) {
	INT ddmin = DP(v,dd).Min();
	FmtAssert(ddmin >= 0, ("Make fully permutable bug"));
	if(ddmin > 0) {
	  ddpos = dd;
	  ddposmin = ddmin;
	  vdist += factors[dd] * ddmin;
	}
	Is_True(ddpos >= 0, ("I'm so confused"));
	if (vdist < 0)
	  factors[ddpos] += (-vdist + ddposmin - 1)/ddposmin;
      }
    }

    const INT maxskew = 3;

    // we've found our skew factors.

    BOOL ok = TRUE;
    for (dd = 0; dd < d; dd++) {
      if (factors[dd] > maxskew) {
	ok = FALSE;
	break;
      }
    }
    if (!ok)
      break;

    for (v = 0; v < vmax; v++) {
      INT dmin = DP(v,d).Min();
      INT d_isconst = !DP(v,d).Unbounded_Max();
      for (dd = 0; dd < d; dd++) {
	if (d_isconst)
	  d_isconst = !DP(v,dd).Unbounded_Max();
	dmin += factors[dd] * DP(v,dd).Min();
      }
      DP(v,d).Distance = dmin;
      DP(v,d).Moreless = d_isconst ? SNL_DEP::SNL_DEP_EXACT : SNL_DEP::SNL_DEP_PLUS;
    }

    // This is a skew.  Do the skew on m.  Only the mth row changes

    INT newrow[SNL_MAX_LOOPS];
    INT i;
    for (i = 0; i < Nloops(); i++) {
      newrow[i] = (*m)(d,i);
      for (dd = 0; dd < d; dd++)
	newrow[i] += factors[dd] * (*m)(dd,i);
    }
    for (i = 0; i < Nloops(); i++)
      (*m)(d,i) = newrow[i];
  }

  CXX_DELETE_ARRAY(deps, &SNL_local_pool);
  CXX_DELETE_ARRAY(per_vector_info, &SNL_local_pool);
  MEM_POOL_Pop_Unfreeze(&SNL_local_pool);

  if (d == Nloops()) {
    FmtAssert(SNL_DEP_MATRIX(*this, Pool()).Is_Fully_Permutable(from, to) == m->Is_Identity(),
	      ("Fully permutable nest if and only if identity transf."));
    return m;
  }
  else {
    CXX_DELETE(m, pool);
    return NULL;
  }
}

//--------------------------------------------------------------------
// SNL_ANAL_INFO member functions
//--------------------------------------------------------------------

static BOOL is_eventually_a_parent(WN* wn, WN* par)
{
  while (wn && wn != par)
    wn = LWN_Get_Parent(wn);
  return wn != NULL;
}

static INT nest_depth(WN* wn)
{
  INT rv;
  for (rv = -1; wn; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP)
      rv++;
  }
  return rv;
}

SNL_ANAL_INFO::SNL_ANAL_INFO(const SNL_NEST_INFO*	ni,
			     BOOL			gtransform,
                             ARRAY_DIRECTED_GRAPH16*    dg,
			     MEM_POOL*			pool)
: _body_deps(ni->Depth_Inner() + 1 - ni->Num_Bad() -
             (gtransform ? ni->Nloops_General() : ni->Nloops_Invariant()),
             gtransform ? ni->Nloops_General() : ni->Nloops_Invariant(),
             ni->Num_Bad(), ni->Dostack(), pool),
  _imperfect_deps(ni->Depth_Inner() + 1 - ni->Num_Bad() -
             (gtransform ? ni->Nloops_General() : ni->Nloops_Invariant()),
             gtransform ? ni->Nloops_General() : ni->Nloops_Invariant(),
             ni->Num_Bad(), ni->Dostack(), pool),
  _lexinfo(HT_ELTS, pool),
  _pool(pool),
  _above_is_distributable(gtransform ? ni->Above_Is_Distributable() : TRUE),
  _below_is_distributable(gtransform ? ni->Below_Is_Distributable() : TRUE),
  _inner_loop(ni->Dostack().Bottom_nth(ni->Depth_Inner())),
  _depth_inner(ni->Depth_Inner()),
  _ci(&ni->Dostack())
{
  // go through collecting refs.

  INT outer_depth = ni->Depth_Inner() + 1 -
    (gtransform ? ni->Nloops_General() : ni->Nloops_Invariant());
  INT lexcount = 0;
  WN* loop = ni->Dostack().Bottom_nth(outer_depth);
  WN* innerloop = ni->Dostack().Bottom_nth(_depth_inner);
  _lex_last_above_innermost = -1;
  _lex_first_below_innermost = -1;

  for (LWN_ITER* wi = LWN_WALK_TreeIter(loop);
       wi; wi = LWN_WALK_TreeNext(wi)) {
    if (_lex_last_above_innermost == -1 && wi->wn == innerloop)
      _lex_last_above_innermost = lexcount;
    OPCODE op = WN_opcode(wi->wn);
    if (OPCODE_is_load(op) || OPCODE_is_store(op) || OPCODE_is_call(op)) {
      if (_lex_last_above_innermost != -1) {
        if (_lex_first_below_innermost == -1) {
	  if (!is_eventually_a_parent(wi->wn, innerloop)) {
	    _lex_first_below_innermost = lexcount + 1;
	    Is_True(nest_depth(wi->wn) < nest_depth(innerloop), ("bug"));
	  }
	  else {
	    Is_True(nest_depth(wi->wn) >= nest_depth(innerloop), ("bug"));
	  }
	}
	else {
	  Is_True(!is_eventually_a_parent(wi->wn, innerloop), ("wierd bug"));
	  Is_True(nest_depth(wi->wn) < nest_depth(innerloop), ("bug"));
	}
      }
      OPERATOR opr = OPCODE_operator(op);
      if (OPCODE_is_load(op) || OPCODE_is_store(op) || OPCODE_is_call(op)) {
        if (dg->Get_Vertex(wi->wn))
          Enter_Lex(wi->wn, LEX_DEPTH(++lexcount, nest_depth(wi->wn)));
        else if (opr != OPR_LDID && opr != OPR_STID) {
          BOOL imperfect = (_lex_last_above_innermost == -1 ||
                            _lex_first_below_innermost != -1);
          SNL_DEP_INFO* dinfo = imperfect ? &_imperfect_deps : &_body_deps;
          INT entry = dinfo->_bad_deps.Newidx();
          dinfo->_bad_deps[entry].e = 0;
          dinfo->_bad_deps[entry].loop = _body_deps.Nloops() - (imperfect?2:1);
          dinfo->_all_stars = TRUE;
        }
      }
    }
  }
  Is_True(_lex_last_above_innermost >= 0, ("Missing inner loop"));
  if (_lex_first_below_innermost == -1)
    _lex_first_below_innermost = lexcount + 1;

  // now go through refs and enter deps in, realizing that when
  // one of the refs not deeply nested enough, have to fake out
  // dependence analyzer.

  HASH_TABLE_ITER<WN*,LEX_DEPTH> lit(&_lexinfo);
  WN*		wn;
  LEX_DEPTH	ld;
  while ((LNO_Analysis || !_body_deps.All_Stars()) && lit.Step(&wn, &ld)) {
    Enter_Deps(wn, ld);
    if (snl_debug >= 3) {
      fprintf(TFile, "snl_deps: insertion of 0x%p (lex depth %d)\n", 
		     wn, ld.Depth);
      Print(TFile);
      fflush(TFile);
    }
  }
}

SNL_ANAL_INFO::~SNL_ANAL_INFO()
{
}

void SNL_ANAL_INFO::Print(FILE* f) const
{
  HASH_TABLE_ITER<WN*,LEX_DEPTH> lit(&_lexinfo);
  WN*		wn;
  LEX_DEPTH	ld;

  INT mrefs;
  for (mrefs = 0; lit.Step(&wn, &ld); mrefs++)
    ;

  fprintf(f, "SNL_ANAL_INFO: <%d memrefs>\n", mrefs);
  Body_Deps().Print(f);
  Imperfect_Deps().Print(f);
  _ci.Print(f);
}

// For each memory reference, look at all edges for which it is the
// source.  Call dependence analysis to recompute the edge if it is
// of non-adequate depth.  Note that we discard the non-lex part, which
// may correspond to an edge in the opposite direction.  So dependence
// analysis is called more than necessary to the extent that edges exist
// from B to A as well as A to B.  This is probably not worth the effort
// to recode, since dependence analysis is only called when one of the
// references is not in the deepest nest anyway.

void SNL_ANAL_INFO::Enter_Deps(WN* wn, LEX_DEPTH ld)
{
  VINDEX16 v = Array_Dependence_Graph->Get_Vertex(wn);
  Is_True(v, ("Source has null vertex"));

  // We only care about dependences when both references are in the nest,
  // and won't be distributed out.  TODO OK: we're being conservative and
  // including references even when it's distributable, unless everything
  // distributable.  Very very small deal.
  // Since each reference within the nest calls this routine, it is
  // adequate just to use the from edges at each call site.

  Is_True(ld.Lex > 0 && ld.Depth >= 0, ("Bad call to Enter_Deps()"));

  INT ab = 1;
  if (Above_Main_Nest(ld.Lex))
    ab = 0;
  else if (Below_Main_Nest(ld.Lex))
    ab = 2;

  EINDEX16 enext = 0;
  for (EINDEX16 e = Array_Dependence_Graph->Get_Out_Edge(v); e; e = enext) {
    enext = Array_Dependence_Graph->Get_Next_Out_Edge(e);
    VINDEX16  v2 = Array_Dependence_Graph->Get_Sink(e);
    Is_True(v2, ("Sink has null vertex"));
    WN*       wn2 = Array_Dependence_Graph->Get_Wn(v2);
    Is_True(wn2, ("Sink has missing WN"));
    LEX_DEPTH ld2 = Find_Lex(wn2);
    INT components = Array_Dependence_Graph->Depv_Array(e)->Num_Dim();

    FmtAssert((components <= _body_deps.First_Component()) ==
              (ld2.Lex == 0), ("Bad component count %d %d %d v%d->v%d",
                                components,
                                _body_deps.First_Component(), ld2.Depth,
                                v, v2));

    // If it's not in the SNL, don't worry about the dependence

    if (ld2.Lex == 0)
      continue;

    // If it's a reduction, don't worry about the dependence.

    if (red_manager &&
	red_manager->Which_Reduction(wn) && 
	red_manager->Which_Reduction(wn2) && 
	(red_manager->Which_Reduction(wn) ==
         red_manager->Which_Reduction(wn2))) {
      continue;
    }

    INT ab2 = 1;
    if (Above_Main_Nest(ld2.Lex))
      ab2 = 0;
    else if (Below_Main_Nest(ld2.Lex))
      ab2 = 2;

    if (components >= _body_deps.First_Component() + _body_deps.Nloops()) {
      _body_deps.Enter(Array_Dependence_Graph->Depv_Array(e), e);
      if (_body_deps.All_Stars())
	return;
    }
    else {
      OPCODE   opc = WN_opcode(wn);
      OPCODE   opc2 = WN_opcode(wn2);
      OPERATOR opr = OPCODE_operator(opc);
      OPERATOR opr2 = OPCODE_operator(opc2);

      if (OPCODE_is_call(opc) || OPCODE_is_call(opc2) ||
          opr == OPR_LDID || opr2 == OPR_LDID  ||
          opr == OPR_STID || opr2 == OPR_STID) {
	_imperfect_deps._all_stars = TRUE; 
	return;
      }
      MEM_POOL_Push_Freeze(&SNL_local_pool);
      DEPV_LIST* dvl = CXX_NEW(DEPV_LIST(wn, wn2,
                                         _imperfect_deps.Stack1().Elements(),
                                         _imperfect_deps.First_Component() +
                                            _imperfect_deps.Nloops(),
                                         TRUE,
                                         &SNL_local_pool,
                                         &_imperfect_deps.Stack1(),
                                         &_imperfect_deps.Stack2()),
                               &SNL_local_pool);

      // clean up dependences inside max common nesting.

      INT missing = _depth_inner - MAX(ld.Depth,ld2.Depth);
      Is_True((missing == 0) == (ab == 1 || ab2 == 1), ("Bug in snl_deps"));
      if (ab != 1 && ab2 != 1) {

	// So, for each dependence vector, components beyond the common nesting
	// may be star, when that's silly.  If the bounds are constant, then
	// we can realize that those dependences are 0 if both references are
	// on the same side of the inner loop, and POSEQ or NEGEQ otherwise.
	// Actually, only need constant bounds when the corresponding
	// dependence is non-zero.

	DEPV_ITER iter(dvl);
	for (DEPV_NODE *n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
	  for (INT d = _imperfect_deps.Nloops() - missing;
               d < _imperfect_deps.Nloops(); d++) {
	    BOOL is_improvable = TRUE;
	    INT depth_this_component = _depth_inner + 1 -
                                       _imperfect_deps.Nloops() + d;
	    if (ab == 0 || ab2 == 0) {
	      for (INT i = 0; is_improvable && i < depth_this_component; i++) {
		if (!_ci.Lbconst(depth_this_component, i)) {
		  INT dc = (_imperfect_deps.Nloops() - 1) - (_depth_inner - i);
		  if (dc < 0 ||
		      DEP_IsDistance(DEPV_Dep(n->Depv, dc)) == FALSE ||
		      DEP_Distance(DEPV_Dep(n->Depv, dc)) != 0)
		    is_improvable = FALSE;
		}
	      }
	    }
	    if (ab == 2 || ab2 == 2) {
	      for (INT i = 0; is_improvable && i < depth_this_component; i++) {
		if (!_ci.Ubconst(depth_this_component, i)) {
		  INT dc = (_imperfect_deps.Nloops() - 1) - (_depth_inner - i);
		  if (dc < 0 ||
		      DEP_IsDistance(DEPV_Dep(n->Depv, dc)) == FALSE ||
		      DEP_Distance(DEPV_Dep(n->Depv, dc)) != 0)
		    is_improvable = FALSE;
		}
	      }
	    }
	    if (is_improvable) {
	      DEPV_Dep(n->Depv, _imperfect_deps.First_Component()+d) =
		(ab == ab2) ? DEP_SetDistance(0) :
	        (ab < ab2) ? DEP_SetDirection(DIR_POSEQ) :
	                     DEP_SetDirection(DIR_NEGEQ);
	    }
	  }
	}
      }

      DEPV_LIST* pos = CXX_NEW(DEPV_LIST(_imperfect_deps.First_Component() +
                                                    _imperfect_deps.Nloops(),
					 _imperfect_deps.Dv_List().Num_Unused_Dim(),
                                         &SNL_local_pool),
                               &SNL_local_pool);
      DEPV_LIST* neg = CXX_NEW(DEPV_LIST(_imperfect_deps.First_Component() +
                                                     _imperfect_deps.Nloops(),
					 _imperfect_deps.Dv_List().Num_Unused_Dim(),
					 &SNL_local_pool),
                               &SNL_local_pool);
      dvl->Lex_Pos_Decompose(&SNL_local_pool, pos, neg,
			    ld.Lex < ld2.Lex, ld.Lex > ld2.Lex);

      if (!pos->Is_Empty()) {
	DEPV_ARRAY *array = Create_DEPV_ARRAY(pos, Pool());
	_imperfect_deps.Enter(array, e);
        if (snl_debug >= 3) {
          fprintf(TFile, "snl_deps: after inserting edge %d: ", e);
          _imperfect_deps.Print(TFile);
        }
      }

      CXX_DELETE(dvl, &SNL_local_pool);
      CXX_DELETE(pos, &SNL_local_pool);
      CXX_DELETE(neg, &SNL_local_pool);

      MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
    }
  }
}

SNL_ANAL_INFO::CONST_BOUNDS_INFO::CONST_BOUNDS_INFO(const DOLOOP_STACK* s)
{
  INT d;
  for (d = 0; d < 64; d++) {
    _lbconst[d] = INT64(-1);
    _ubconst[d] = INT64(-1);
  }

  for (d = 0; d < MIN(s->Elements(),64); d++) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(s->Bottom_nth(d));
    ACCESS_ARRAY* aalb = dli->LB;
    ACCESS_ARRAY* aaub = dli->UB;

    if (aalb->Too_Messy)
      _lbconst[d] = 0;
    else {
      for (INT dimlb = 0; dimlb < aalb->Num_Vec(); dimlb++) {
	ACCESS_VECTOR* av = aalb->Dim(dimlb);
	if (av->Too_Messy || av->Contains_Non_Lin_Symb())
	  _lbconst[d] = 0;
	else {
	  for (INT dd = 0; dd < d; dd++)
	    if (av->Loop_Coeff(dd))
	      _lbconst[d] &= ~(INT64(1)<<dd);
	  Is_True(av->Loop_Coeff(d), ("Bad access vector"));
	}
      }
    }

    if (aaub->Too_Messy)
      _ubconst[d] = 0;
    else {
      for (INT dimub = 0; dimub < aaub->Num_Vec(); dimub++) {
	ACCESS_VECTOR* av = aaub->Dim(dimub);
	if (av->Too_Messy || av->Contains_Non_Lin_Symb())
	  _ubconst[d] = 0;
	else {
	  for (INT dd = 0; dd < d; dd++)
	    if (av->Loop_Coeff(dd))
	      _ubconst[d] &= ~(INT64(1)<<dd);
	  Is_True(av->Loop_Coeff(d), ("Bad access vector"));
	}
      }
    }
  }
}

void SNL_ANAL_INFO::CONST_BOUNDS_INFO::Print(FILE* f) const
{
  fprintf(f, "CONST BOUNDS INFO:\n");
  for (INT i = 0; i < 64; i++) {
    if (_lbconst[i] == INT64(-1) && _ubconst[i] == INT64(-1))
      continue;

    fprintf(f, "Indices modified at depth %d:", i);
    fprintf(f, " LB:");
    INT ii;
    for (ii = 0; ii < i; ii++) {
      if (Lbconst(i,ii) == FALSE)
	fprintf(f, " %d", ii);
    }
    fprintf(f, " UB:");
    for (ii = 0; ii < i; ii++) {
      if (Ubconst(i,ii) == FALSE)
	fprintf(f, " %d", ii);
    }
    fprintf(f, "\n");
  }
}


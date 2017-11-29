//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_dom.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dom.cxx,v $
//
// Revision history:
//  20-SEP-94 - Original Version
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
// Description:
//
// Compute the dominators and post-dominators of a compilation unit.
//
// The algorithm is identical to the one used in "A Fast Algorithm
// for Finding Dominators in a Flowgraph" in ACM Trans. on Programming
// Languages and Systems, Vol. 1, No. 1, July 1979, by Lengauer and
// Tarjan.
//
// The algorithm assumes that nodes in the control-flow graph are
// numbered 1..n.  If they aren't, we could provide a mapping such
// that we internally have a number for each node in this range.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_dom_CXX	"opt_dom.cxx"
static char *rcs_id = 	opt_dom_CXX"$Revision$";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "mempool.h"
#include "tracing.h"
#include "cxx_memory.h"

#include "opt_sys.h"
#include "opt_cfg.h"
#include "opt_defs.h"
#include "bb_node_set.h"

// ====================================================================
//  Print_dom
//  Print_pdom
//
//  Print the dominator/post-dominator information for a given CFG
//  NOTE: There is a default initializer for 'fp'
// ====================================================================

static void
Dom_print_dom( BB_NODE *bb, BOOL print_dom, FILE *fp )
{
  if ( bb != NULL ) {
    BOOL print_kid;
    BB_LIST_ITER bb_iter;
    BB_NODE *bb_kid;

    fprintf( fp, "BB:%-5d", bb->Id() );

    // print the kids, if any
    print_kid = TRUE;
    FOR_ALL_ELEM( 
      bb_kid, bb_iter, Init(print_dom?bb->Dom_bbs():bb->Pdom_bbs()) )
    {
      if ( print_kid ) {
	fprintf( fp, " kids: " );
	print_kid = FALSE;
      }
      if ( bb_kid != NULL ) {
	fprintf( fp, "bb:%-5d ", bb_kid->Id() );
      }
    }
    // finished printing information for this dominator
    fprintf( fp, "\n" );

    // recursively visit the kids
    FOR_ALL_ELEM( 
      bb_kid, bb_iter, Init(print_dom?bb->Dom_bbs():bb->Pdom_bbs()) )
    {
      Dom_print_dom( bb_kid, print_dom, fp );
    }
  }
  else {
    fprintf( fp, "<null bb_node>\n" );
  }
}

static void
Print_dom( const CFG *cfg, FILE *fp=stderr )
{
  fprintf ( fp, "Print_dom\n" );
  Dom_print_dom( cfg->Entry_bb(), TRUE, fp );
}

static void
Print_pdom( const CFG *cfg, FILE *fp=stderr )
{
  fprintf ( fp, "Print_pdom\n" );
  Dom_print_dom( cfg->Exit_bb(), FALSE, fp );
}


// ====================================================================
// ====================================================================
//
//  CFG::Compute_dom_tree
//
//  Create the dominator (or post-dominator) tree for the given 
//  control-flow graph.
//
// ====================================================================
// ====================================================================


//  The DOM_INFO class is used strictly for computing the dominator
//  tree.  The paper describing the algorithm also describes these
//  fields.

class DOM_INFO {
  private:

    class DOM_REC {
      friend class DOM_INFO;
      DECL_CXX_ALLOC_AS_FRIEND(DOM_REC);
      private:
	IDTYPE	parent;		// block which is parent of this block
	IDTYPE	ancestor;	// furthest ancestor in the tree
	IDTYPE	child;		// used in the link method
	IDTYPE	vertex;		// block this info is for
	IDTYPE	dom;		// after step 4, immed dom of this block
	IDTYPE	label;		// used in link/compress methods
	IDTYPE	semi;		// number of semi-dom of this block
	IDTYPE	size;		// used in link method
	BB_NODE_SET	*bucket;// semi-doms of this block

	// constructors and destructors
	DOM_REC(void)	{}	// DON'T USE THIS (except for arrays)
	DOM_REC(const DOM_REC&);
	~DOM_REC(void)	{}
	DOM_REC &operator = (const DOM_REC&);

	void Init( const CFG *cfg, MEM_POOL *mempool );
    }; // end of DOM_REC

    IDTYPE	counter;	// used to number blocks in DFS
    DOM_REC    *recs;		// array of DOM_RECs

    // private constructor so it cannot be used
    DOM_INFO(void);
    DOM_INFO(const DOM_INFO&);
    DOM_INFO& operator = (const DOM_INFO&);

    // depth first search going down from entry block
    void eDFS( BB_NODE *bbv );
    // depth first search going up from exit block
    void xDFS( BB_NODE *bbv );

    void Compress( IDTYPE v );
    IDTYPE Eval( const BB_NODE *bbv );
    void Link( BB_NODE *bbv, BB_NODE *bbw );

    void Build_dom_tree( CFG *cfg );
    void Build_pdom_tree( const CFG *cfg );

  public:

    DOM_INFO( const CFG *cfg, MEM_POOL *mempool );
    ~DOM_INFO( void ) {}

    IDTYPE	Counter( void )      const { return counter; }
    IDTYPE	Parent( IDTYPE n )   const { return recs[n].parent; }
    IDTYPE	Ancestor( IDTYPE n ) const { return recs[n].ancestor; }
    IDTYPE	Child( IDTYPE n )    const { return recs[n].child; }
    IDTYPE	Vertex( IDTYPE n )   const { return recs[n].vertex; }
    IDTYPE	Dom( IDTYPE n )      const { return recs[n].dom; }
    IDTYPE	Label( IDTYPE n )    const { return recs[n].label; }
    IDTYPE	Semi( IDTYPE n )     const { return recs[n].semi; }
    IDTYPE	Size( IDTYPE n )     const { return recs[n].size; }

    BB_NODE_SET *Bucket( IDTYPE n )  const { return recs[n].bucket; }

    void Set_counter(IDTYPE c)		   { counter = c; }
    void Set_parent(IDTYPE n, IDTYPE p)    { recs[n].parent = p; }
    void Set_ancestor(IDTYPE n, IDTYPE a)  { recs[n].ancestor = a; }
    void Set_child(IDTYPE n, IDTYPE c)     { recs[n].child = c; }
    void Set_vertex(IDTYPE n, IDTYPE v)    { recs[n].vertex = v; }
    void Set_dom(IDTYPE n, IDTYPE d)       { recs[n].dom = d; }
    void Set_label(IDTYPE n, IDTYPE l)     { recs[n].label = l; }
    void Set_semi(IDTYPE n, IDTYPE s)      { recs[n].semi = s; }
    void Set_size(IDTYPE n, IDTYPE s)      { recs[n].size = s; }
    void Set_bucket(IDTYPE n, BB_NODE_SET *b){ recs[n].bucket = b; }

    // compute a dominator tree (build_dom=TRUE to build dominators,
    //   and build_dom=FALSE to build post-dominators)
    void Compute_dom_tree( CFG *cfg, BOOL build_dom );
};

// ====================================================================
//  DOM_REC::Init
//
//  WARNING: assumed that all of the other fields have been cleared
//  already when the structure was allocated. (i.e., do not create
//  one of these on the stack)
// ====================================================================

void
DOM_INFO::DOM_REC::Init( const CFG *cfg, MEM_POOL *mempool )
{
  // create bucket sets large enough to hold largest Id() so these
  //   sets don't need to grow while the algorithm runs
  IDTYPE bbs = cfg->Last_bb_id()+1;
  bucket = CXX_NEW(BB_NODE_SET(bbs,cfg,mempool,BBNS_EMPTY),mempool);
}

// ====================================================================
//  DOM_INFO constructor/destructor
// ====================================================================

DOM_INFO::DOM_INFO( const CFG *cfg, MEM_POOL *mempool )
{
  IDTYPE bbs = cfg->Last_bb_id()+1;// alloc 1 more to index by max_bbid

  counter  = 0;
  recs = CXX_NEW_ARRAY(DOM_REC, bbs, mempool);

  // clear out the array
  // WARNING: I'm assuming that no real constructor is called for
  // each element of the array
  BZERO( recs, bbs*sizeof(recs[0]) );

  // now init anything that doesn't need to be zero
  for ( INT i = 0; i < bbs; i++ ) {
    recs[i].Init(cfg,mempool);
  }
}

// ====================================================================
//  DOM_INFO::eDFS
//
//  Perform pre-order numbering of CFG starting at its entry node
// ====================================================================

void
DOM_INFO::eDFS( BB_NODE *bbv )
{
  IDTYPE v = bbv->Id();
  BB_NODE *succ;
  BB_LIST_ITER bb_succ_iter;
  Is_True(v!=0, ("DOM_INFO::eDFS: Uninitialized BB id, Bad WHIRL input!"));
  counter++;
  Set_semi(v,counter);
  Set_vertex(counter,v);
  Set_label(v,v);
  Set_ancestor(v,0);
  Set_child(v,0);
  Set_size(v,1);

  // clear out BB_NODE dominator information
  bbv->Set_idom(NULL);
  bbv->Set_dom_bbs(NULL);

  FOR_ALL_ELEM( succ, bb_succ_iter, Init(bbv->Succ()) ) {
    IDTYPE succ_id = succ->Id();
    if ( Semi(succ_id) == 0 ) {
      Set_parent(succ_id,v);
      eDFS( succ );
    }
  }
}

// ====================================================================
//  DOM_INFO::xDFS
//
//  Perform pre-order numbering of CFG starting at its exit node
// ====================================================================

void
DOM_INFO::xDFS( BB_NODE *bbv )
{
  IDTYPE v = bbv->Id();
  BB_NODE *pred;
  BB_LIST_ITER bb_pred_iter;

  counter++;
  Set_semi(v,counter);
  Set_vertex(counter,v);
  Set_label(v,v);
  Set_ancestor(v,0);
  Set_child(v,0);
  Set_size(v,1);

  // clear out BB_NODE post-dominator information
  bbv->Set_ipdom(NULL);
  bbv->Set_pdom_bbs(NULL);

  FOR_ALL_ELEM( pred, bb_pred_iter, Init(bbv->Pred()) ) {
    IDTYPE pred_id = pred->Id();
    if ( Semi(pred_id) == 0 ) {
      Set_parent(pred_id,v);
      xDFS( pred );
    }
  }
}

// ====================================================================
//  DOM_INFO::Compress
// ====================================================================

void
DOM_INFO::Compress( IDTYPE v )
{
  Assert( Ancestor(v) != 0, 
	  (EC_Unimplemented, "Ancestor(v) is 0") );

  if ( Ancestor(Ancestor(v)) != 0 ) {
    Compress( Ancestor(v) );

    if ( Semi(Label(Ancestor(v))) < Semi(Label(v)) ) {
      Set_label(v, Label(Ancestor(v)));
    }

    Set_ancestor(v, Ancestor(Ancestor(v)));
  }
}

// ====================================================================
//  DOM_INFO::Eval
// ====================================================================

IDTYPE
DOM_INFO::Eval( const BB_NODE *bbv )
{
  IDTYPE v = bbv->Id();

  if ( Ancestor(v) == 0 ) {
    return ( Label(v) );
  }
  else {
    Compress( v );

    return ( (Semi(Label(Ancestor(v))) >= Semi(Label(v))) ?
	     Label(v) : Label(Ancestor(v)) );
  }
}

// ====================================================================
//  DOM_INFO::Link
// ====================================================================

void
DOM_INFO::Link( BB_NODE *bbv, BB_NODE *bbw )
{
  IDTYPE v = bbv->Id();
  IDTYPE w = bbw->Id();
  IDTYPE s = w;

  while ( Semi(Label(w)) < Semi(Label(Child(s))) ) {
    if ( Size(s) + Size(Child(Child(s))) >= 2*Size(Child(s)) ) {
      Set_ancestor(Child(s), s);
      Set_child(s, Child(Child(s)));
    }
    else {
      Set_size(Child(s), Size(s));
      Set_ancestor(s, Child(s));
      s = Child(s);
    }
  }

  Set_label(s, Label(w));
  Set_size(v, Size(v) + Size(w));

  if ( Size(v) < 2*Size(w) ) {
    // swap s and child(v)
    IDTYPE tmps = s;
    s = Child(v);
    Set_child(v, tmps);
  }

  while ( s != 0 ) {
    Set_ancestor(s, v);
    s = Child(s);
  }
}

// ====================================================================
//  DOM_INFO::Build_dom_tree
// ====================================================================

void
DOM_INFO::Build_dom_tree( CFG *cfg )
{
  // Note that the construction of this iterator entails a depth-first
  // search of the CFG if CFG::_dfs_vec isn't already built, and we
  // have already done such a search in DOM_INFO::eDFS. It's a shame
  // some effort goes to waste in this area, but I don't immediately
  // see a clean and easy way to save the waste.
  DFSBB_ITER dfs_iter(cfg);
  BB_NODE *bb;

  FOR_ALL_ELEM( bb, dfs_iter, Init() ) {
    IDTYPE bbid = bb->Id();

    if ( Dom(bbid) != 0 ) {
      BB_NODE *idombb = cfg->Get_bb(Dom(bbid));

      // this bb is immediately dominated by idombb
      bb->Set_idom(idombb);

      // and similarly, idombb dominates bb
      idombb->Add_dom_bbs(bb, cfg->Mem_pool());
    }
    else {
      // only the entry and fake-exit blocks are allowed to not have a
      // dominator
      FmtAssert( bb == cfg->Entry_bb() || 
		 bb == cfg->Fake_entry_bb() ||
		 bb == cfg->Fake_exit_bb(),
	("DOM_INFO::Build_dom_tree: No dom for BB_NODE %d", bbid) );
    }
  }
}

// ====================================================================
//  DOM_INFO::Build_pdom_tree
// ====================================================================

void
DOM_INFO::Build_pdom_tree( const CFG *cfg )
{
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;

  FOR_ALL_NODE( bb, cfg_iter, Init() ) {
    IDTYPE bbid = bb->Id();

    if ( Dom(bbid) != 0 ) {
      BB_NODE *ipdombb = cfg->Get_bb(Dom(bbid));

      // this bb is immediately post-dominated by ipdombb
      bb->Set_ipdom(ipdombb);

      // and similarly, ipdombb post-dominates bb
      ipdombb->Add_pdom_bbs(bb, cfg->Mem_pool());
    }
    else {
      // only the exit and fake entry blocks are allowed to not have 
      // a post-dominator
      FmtAssert( bb == cfg->Exit_bb() || 
		 bb == cfg->Fake_exit_bb() ||
		 bb == cfg->Fake_entry_bb(),
      ("DOM_INFO::Build_pdom_tree: No post-dom for BB_NODE %d", bbid) );
    }
  }
}

// ====================================================================
//  DOM_INFO::Compute_dom_tree
//
//  build_dom == TRUE  if we should build dominator tree
//  build_dom == FALSE if we should build post-dominator tree
// ====================================================================

void
DOM_INFO::Compute_dom_tree( CFG *cfg, BOOL build_dom )
{
  // step 1
  //   note: counter, pred, bucket, and semi are initialized when 
  //   allocated
  if ( build_dom ) {
    eDFS( cfg->Entry_bb() );
  }
  else {
    xDFS( cfg->Exit_bb() );
  }

  // ? is this re-initialization needed ?
  Set_size(0,0);
  Set_label(0,0);
  Set_semi(0,0);

  for ( IDTYPE i = Counter(); i >= 2; i-- ) {
    IDTYPE w = Vertex(i);
    Is_True(w!=0, ("DOM_INFO::Compute_dom_tree, algorithm failed, the input tree is bad!"));
    BB_NODE *bbw = cfg->Get_bb(w);
    BB_NODE *bbv;

    // step 2
    if ( build_dom ) {
      BB_LIST_ITER bbw_pred_iter;
      // for each predecessor of w
      FOR_ALL_ELEM( bbv, bbw_pred_iter, Init(bbw->Pred()) ) {
	IDTYPE u = Eval( bbv );
	if ( Semi(u) < Semi(w) ) {
	  Set_semi(w, Semi(u));
	}
      }
    }
    else {
      BB_LIST_ITER bbw_succ_iter;
      // for each successor of w
      FOR_ALL_ELEM( bbv, bbw_succ_iter, Init(bbw->Succ()) ) {
	IDTYPE u = Eval( bbv );
	if ( Semi(u) < Semi(w) ) {
	  Set_semi(w, Semi(u));
	}
      }
    }

    // add w to bucket(vertex(semi(w)))
    Bucket(Vertex(Semi(w)))->Union1D( bbw );

    Link( cfg->Get_bb(Parent(w)), bbw );

    // step 3
    // for each v in bucket(parent(w))
    {BB_NODE_SET_ITER par_w_iter;
    FOR_ALL_ELEM( bbv, par_w_iter, Init(Bucket(Parent(w))) ) {
      IDTYPE v = bbv->Id();
      IDTYPE u = Eval( bbv );

      // delete v from bucket(parent(w))
      Bucket(Parent(w))->Difference1D( bbv );

      Set_dom(v, (Semi(u) < Semi(v)) ? u : Parent(w));
    }}

  } // end reverse loop through Vertex() */

  // step 4
  for ( IDTYPE j = 2; j <= Counter(); j++ ) {
    IDTYPE w = Vertex(j);
    if ( Dom(w) != Vertex(Semi(w)) ) {
      Set_dom(w, Dom(Dom(w)));
    }
  }

  if ( build_dom ) {
    // root of control-flow graph doesn't have a dominator
    Set_dom(cfg->Entry_bb()->Id(), 0);

    // Actually build the dominator tree for what we've calculated
    Build_dom_tree( cfg );

    // Tracing output of the dominator tree?
    if ( Get_Trace( TP_GLOBOPT, DOM_DUMP_FLAG ) ) {
      Print_dom( cfg, TFile );
    }
  }
  else {
    // exit-root of control-flow graph doesn't have a post-dominator
    Set_dom(cfg->Exit_bb()->Id(), 0);

    // Actually build the post-dominator tree for what we've calculated
    Build_pdom_tree( cfg );

    // Tracing output of the post-dominator tree?
    if ( Get_Trace( TP_GLOBOPT, DOM_DUMP_FLAG ) ) {
      Print_pdom( cfg, TFile );
    }
  }

}

// ====================================================================
//  CFG::Compute_dom_dfs_id
//  Compute the values of dom_dfs_id and dom_dfs_last for each bb
//
//  CFG::Compute_pdom_dfs_id
//  Compute the values of pdom_dfs_id and pdom_dfs_last for each bb
// ====================================================================

static void
Compute_dom_dfs_id( BB_NODE *bb, IDTYPE *id )
{
  bb->Set_dom_dfs_id(*id);
  ++(*id);

  BB_NODE     *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM ( dom_bb, dom_bb_iter, Init(bb->Dom_bbs()) )
    Compute_dom_dfs_id(dom_bb, id);

  bb->Set_dom_dfs_last(*id - 1);
}

static void
Compute_pdom_dfs_id( BB_NODE *bb, IDTYPE *id )
{
  bb->Set_pdom_dfs_id(*id);
  ++(*id);

  BB_NODE     *pdom_bb;
  BB_LIST_ITER pdom_bb_iter;
  FOR_ALL_ELEM ( pdom_bb, pdom_bb_iter, Init(bb->Pdom_bbs()) )
    Compute_pdom_dfs_id(pdom_bb, id);

  bb->Set_pdom_dfs_last(*id - 1);
}

// ====================================================================
//  CFG::Compute_dom_tree
//
//  build_dom == TRUE  if we should build dominator tree
//  build_dom == FALSE if we should build post-dominator tree
// ====================================================================

void
CFG::Compute_dom_tree( BOOL build_dom )
{
  DOM_INFO *dom_info;

  OPT_POOL_Push( _loc_pool, DOM_DUMP_FLAG );

  dom_info = CXX_NEW(DOM_INFO(this, _loc_pool), _loc_pool);
  dom_info->Compute_dom_tree(this, build_dom);

  OPT_POOL_Pop( _loc_pool, DOM_DUMP_FLAG );

  IDTYPE id = 0;
  if (build_dom) {
//     CFG_ITER cfg_iter(this);
//     BB_NODE *bb;
//     FOR_ALL_NODE( bb, cfg_iter, Init() )
//       bb->Set_dom_dfs_id0(0);             // unnecessary initialization?
    Compute_dom_dfs_id( Entry_bb(), &id );
  } else {
//     CFG_ITER cfg_iter(this);
//     BB_NODE *bb;
//     FOR_ALL_NODE( bb, cfg_iter, Init() )
//       bb->Set_pdom_dfs_id0(0);            // unnecessary initialization?
    Compute_pdom_dfs_id( Exit_bb(), &id );
  }
}

// ====================================================================
// ====================================================================
//
//  CFG::Compute_dom_frontier
//
//  Compute the dominance frontier based upon the control-flow graph
//  and associated dominator tree
//
// ====================================================================
// ====================================================================

// ====================================================================
//  Dom_compute_dom_frontier (recursive function)
// ====================================================================

static void
Dom_compute_dom_frontier( const CFG *cfg, BB_NODE *bbx, MEM_POOL *mem_pool )
{
  BB_LIST_ITER bbx_iter;
  BB_NODE *bby, *bbz;
  BB_NODE_SET *dfx;

  // we need to do a bottom up traversal, so visit dominated blocks
  // before doing anything with this block
  FOR_ALL_ELEM( bby, bbx_iter, Init(bbx->Dom_bbs()) ) {
    Dom_compute_dom_frontier( cfg, bby, mem_pool );
  }

  // start with empty set
  dfx = CXX_NEW(BB_NODE_SET(0, cfg, mem_pool, BBNS_EMPTY),
		mem_pool);

  // for each Y element of Successor(X)
  FOR_ALL_ELEM( bby, bbx_iter, Init(bbx->Succ()) ) {
    BB_NODE *idomy = bby->Idom();

    if ( idomy != bbx ) {
      // add Y to the dominance frontier of X
      dfx->Union1D( bby );
    }
  }

  // for each Z element of Dominated-blocks-of(X)
  FOR_ALL_ELEM( bbz, bbx_iter, Init(bbx->Dom_bbs()) ) {
    BB_NODE_SET *dfz = bbz->Dom_frontier();
    BB_NODE_SET_ITER dfz_iter;

    // for each Y element of Dominance-Frontier(Z)
    FOR_ALL_ELEM( bby, dfz_iter, Init(dfz) ) {
      BB_NODE *idomy = bby->Idom();

      if ( idomy != bbx ) {
	// add Y to the dominance frontier of X
	dfx->Union1D( bby );
      }
    }
  }

  // put the dominance frontier into the bb structure
  bbx->Set_dom_frontier( dfx );
  
  if ( Get_Trace( TP_GLOBOPT, DOM_DUMP_FLAG ) ) {
    fprintf ( TFile, "DF(BB:%d): ", bbx->Id() );
    dfx->Print( TFile );
    fprintf ( TFile, "\n" );
  }
}

// ====================================================================
//  CFG::Compute_dom_frontier
// ====================================================================

void
CFG::Compute_dom_frontier( void )
{
  // start at the root of the dominator tree
  Assert( Entry_bb()->Dom_bbs() != NULL,
	  (EC_Unimplemented,"CFG::Compute_dom_frontier: no dominator"));
  Dom_compute_dom_frontier( this, Entry_bb(), _mem_pool );
}


// ====================================================================
//  Dom_compute_rcfg_dom_frontier (recursive function)
// ====================================================================
static void
Dom_compute_rcfg_dom_frontier( const CFG *cfg, BB_NODE *block, MEM_POOL *mem_pool )
{
  BB_LIST_ITER	block_iter;
  BB_NODE	*y, *z;

  // we need to do a bottom up traversal of the post dominated blocks
  FOR_ALL_ELEM( y, block_iter, Init(block->Pdom_bbs()) )
  {
    Dom_compute_rcfg_dom_frontier( cfg, y, mem_pool );
  }

  // initialize needed bits sets
  block->Set_rcfg_dom_frontier (
    CXX_NEW(BB_NODE_SET(0, cfg, mem_pool, BBNS_EMPTY), mem_pool));

  // for Pred that do not immediately post dominate add to the local
  // contribution of the rcfg dom frontier
  FOR_ALL_ELEM( y, block_iter, Init(block->Pred()) ) {
    if ( y->Ipdom() != block ) {
      block->Rcfg_dom_frontier()->Union1D( y );
    }
  }

  // global contributions from the already processed post dominator blocks
  FOR_ALL_ELEM( z, block_iter, Init(block->Pdom_bbs()) ) {
    BB_NODE_SET_ITER	dfz_iter;
    BB_NODE_SET		*dfz = z->Rcfg_dom_frontier();

    // for each Y element of RCFG Dominance-Frontier
    FOR_ALL_ELEM( y, dfz_iter, Init(dfz) ) {
      if ( y->Ipdom() != block ) {
	block->Rcfg_dom_frontier()->Union1D( y );
      }
    }
  }

  // put the dominance frontier into the bb structure
  if ( Get_Trace( TP_GLOBOPT, DOM_DUMP_FLAG ) ) {
    block->Print(TFile);
  }
}

// ====================================================================
// ====================================================================
//
//  CFG::Compute_control_dependence
//
//  Compute the control dependence based upon the dominance frontiers
//  of the reverse graph of the CFG
//
//  This requires computing the dominance frontier of the reverse
//  control flow graph (rcfg) and then inverting the list
//
//  We compute the dominance frontier without actually computing the rcfg
//
// ====================================================================
// ====================================================================
void
CFG::Compute_control_dependence( void )
{
  CFG_ITER cfg_iter;

  // start at the root of the dominator tree
  Assert( Entry_bb()->Dom_bbs() != NULL,
	  (EC_Unimplemented,"CFG::Compute_control_dependence: no dominator"));

  Dom_compute_rcfg_dom_frontier( this, Exit_bb(), _mem_pool );

}

